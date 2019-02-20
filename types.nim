import sequtils, strutils, strformat, tables, sugar, hashes, gen_kind, sets, json, macros, terminal, helpers
import
  compiler/[ast, astalgo, idents, msgs, renderer, lineinfos]

type
  T* {.pure.} = enum 
    Simple,
    Method,
    MethodOverload,
    Compound,
    Generic,
    Object,
    Tuple,
    GenericVar,
    Union,
    Macro,
    Any

  Type* = ref object
    label*:     string
    fullLabel*: string # with namespace
    args*:     seq[Type]
    isRef*:    bool
    isVar*:    bool
    case kind*: T:
    of T.Simple:
      # Types of atom values and types for which we only know the name, not the structure
      extra*: string
    of T.Method:
      # args
      returnType*:   Type
      # effects
    of T.MethodOverload:
      # Overloads
      overloads*:   seq[Type]
    of T.Compound:
      # Instantiation of a generic type
      # args
      original*: Type
    of T.Generic:
      # Generic type
      genericArgs*: seq[string]
    of T.Object:
      # The fields of an object type
      base*:      Type
      inherited*: bool
      init*:      string # if empty, :
      fields*:   Table[string, Type]
    of T.Tuple, T.Union:
      # A tuple
      elements*: seq[Type]
    of T.GenericVar:
      # A generic
      discard
    of T.Macro:
      macroArgs*: seq[Type]
    of T.Any:
      # Universal
      discard

  Pass* {.pure.} = enum AST, Generation


  
  Compiler* = object
    db*: TraceDB
    command: string
    asts*: Table[string, Node]
    modules*: Table[string, Module]
    maybeModules*: Table[string, bool]
    stack*: seq[(string, seq[(Type, string)])]
    path*: string
    envs*: Table[string, Env]
    constants*: Table[string, Type]
    untilPass*: Pass
    generated*: Table[string, string]
    currentModule*: string
    currentClass*: Type
    currentFunction*: string
    currentCalls*: HashSet[string]
    base*: string
    depth*: int
    identifierCollisions*: Table[string, (string, bool)] # idiomaticIdentifier, (existingIdentifier, collision)
                                                          # if there is already a different existing identifier
                                                          # for that idiomatic one, toggle collision to true
  TraceDB* = ref object
    root:         JsonNode
    modules*:     seq[string]
    types*:       Table[string, Type]
    sysPath*:     seq[string]
    projectDir*:  string
    package*:     string
  
  Env* = ref object
    types*:       Table[string, Type]
    args*:        Table[string, bool]
    returnType*:  Type
    label*:       string
    parent*:      Env
    top*:         Env
    hasYield*:    bool

  NodeKind* = enum
    Class, NodeMethod, Call, Variable, Int, Send, Assign,
    RubySend, RubyInt,
    PyAST, PyAdd, PyAnd, PyAnnAssign, PyAssert, PyAssign, PyAsyncFor, PyAsyncFunctionDef, PyAsyncWith, PyAttribute,
    PyAugAssign, PyAugLoad, PyAugStore, PyAwait, PyBinOp, PyBitAnd, PyBitOr, PyBitXor, PyBoolOp, PyBreak, PyBytes,
    PyCall, PyClassDef, PyCompare, PyConstant, PyContinue, PyDel, PyDelete, PyDict,
    PyDictComp, PyDiv, PyEllipsis, PyEq, PyExceptHandler, PyExpr, PyExpression,
    PyExtSlice, PyFloorDiv, PyFor, PyFormattedValue, PyFunctionDef, PyGeneratorExp, PyGlobal, PyGt, PyGtE,
    PyIf, PyIfExp, PyImport, PyImportFrom, PyIn, PyIndex, PyInteractive, PyInvert, PyIs, PyIsNot,
    PyJoinedStr, PyLShift, PyLambda, PyList, PyListComp, PyLoad, PyLt, PyLtE, Sequence,
    PyMatMult, PyMod, PyModule, PyMult,
    PyName, PyLabel, PyNameConstant, PyNodeTransformer, PyNodeVisitor, PyNonlocal, PyNot, PyNotEq, PyNotIn, PyInt, PyFloat, PyNone,
    PyOr, PyParam, PyPass, PyPow, PyPyCF_ONLY_AST, PyRShift, PyRaise, PyReturn,
    PySet, PySetComp, PySlice, PyStarred, PyStore, PyStr, PySub, PySubscript, PySuite,
    PyTry, PyTuple,
    PyUAdd, PyUSub, PyUnaryOp, PyWhile, PyWith, PyYield, PyYieldFrom, Py_NUM_TYPES, Pyalias, Pyarguments, Pyarg, Pykeyword, Pycomprehension, Pywithitem,
    PyOperator, PyVarDef, PyChar, PyConstr, NimWhen, PyHugeInt, NimRange, NimRangeLess, NimCommentedOut, NimExprColonExpr, NimInfix, NimAccQuoted, NimOf, NimPrefix, NimIf, NimElif, NimElse, NimTuple

  Declaration* {.pure.} = enum Existing, Let, Var, Const

  Node* = ref object
    typ*: Type # The nim type of the node
    debug*: string # Eventually python source?
    idiomatic*: seq[int] # list of idioms applied
    line*: int # Line, -1 or actual
    column*: int # Column, -1 or actual
    label*: string
    isFinished*: bool
    isReplace*: bool
    case kind*: NodeKind:
    of PyStr, PyBytes:
      text*: string
    of Int, PyInt:
      i*: int
    of PyFloat:
      f*: float
    of PyChar:
      c*: char
    of PyHugeInt:
      h*: string
    of PyAssign:
      declaration*: Declaration
    of PyImport:
      aliases*: seq[Node]
    of PyFunctionDef:
      calls*: HashSet[string]
    of Class:
      fields*: seq[Field]
      methods*: seq[Field]
      docstring*: seq[string]
    of NodeMethod:
      # id*: string
      isIterator*: bool
      isMethod*: bool
      isGeneric*: bool
      returnType*: Type
      args*: seq[Node]
      code*: seq[Node]
      doc*: seq[string]
    else:
      discard
    children*: seq[Node] # complicates everything to have it disabled for several nodes

  Field* = object
    label*: string
    node*: Node

  Module* = object
    name*: string
    imports*: seq[Node] # probably Import and Assign
    types*: seq[Node] # probably ClassDef
    methods*: seq[Node] # probably FunctionDef
    main*: seq[Node] # other top level stuff
  
  TypeDependency* = object
    methods*: Table[string, seq[string]]
    ignore*:  seq[string]
    all*:     seq[string]

  Python2NimError* = object of Exception

  NimVersion* {.pure.} = enum V019, Development

  Generator* = object
    indent*:               int
    v*:                    NimVersion
    module*:               Module
    identifierCollisions*: HashSet[string]
    res*:                  PNode

let endl = "\n"

proc simpleType*(label: string): Type =
  Type(kind: T.Simple, label: label)

proc `$`*(t: Type): string

proc hash*(t: Type): Hash =
  var h: Hash = 0
  h = h !& hash(t.kind)
  h = h !& hash(t.label)
  result = !$h

proc `==`*(t: Type, u: Type): bool =
  # structural equivalence for some, nominal for objects
  let tptr = cast[pointer](t)
  let uptr = cast[pointer](u)

  if tptr == nil:
    return uptr == nil
  if uptr == nil or t.kind != u.kind:
    return false

  case t.kind:
  of T.Simple:
    result = t.label == u.label
  of T.Method:
    result = len(t.args) == len(u.args) and zip(t.args, u.args).allIt(it[0] == it[1]) and t.returnType == u.returnType
  of T.MethodOverload:
    result = t.fullLabel == u.fullLabel
  of T.Compound:
    result = t.original == u.original and len(t.args) == len(u.args) and zip(t.args, u.args).allIt(it[0] == it[1])
  of T.Generic:
    result = t.label == u.label and len(t.genericArgs) == len(u.genericArgs)
  of T.Object:
    result = t.fullLabel == u.fullLabel
  of T.Tuple, T.Union:
    result = len(t.elements) == len(u.elements) and zip(t.elements, u.elements).allIt(it[0] == it[1])
  of T.GenericVar:
    result = t.label == u.label
  of T.Macro:
    result = false
  of T.Any:
    result = true

proc dump*(t: Type, depth: int): string =
  var offset = repeat("  ", depth)
  var left = if t.isNil: "nil" else: ""
  if left == "":
    left = case t.kind:
      of T.Simple:
        fmt"type {$t.label}"
      of T.Method:
        let args = t.args.mapIt(dump(it, 0)).join(" ")
        fmt"({args}) -> {dump(t.returnType, 0)}"
      of T.MethodOverload:
        let overloads = t.overloads.mapIt(dump(it, depth + 1)).join("\n")
        fmt"{t.fullLabel}:{endl}{overloads}"
      of T.Compound:
        "$1[$2]" % [if len(t.original.label) == 0: "?" else: t.original.label, t.args.mapIt(dump(it, 0)).join(" ")]
      of T.Generic:
        "$1[$2]" % [t.label, t.genericArgs.join(" ")]
      of T.Object:
        var members = ""
        for label, member in t.fields:
          members.add("$1$2: $3\n" % [repeat("  ", depth + 1), label, dump(member, 0)])
        "$1:\n$2" % [t.label, members]
      of T.Tuple, T.Union:
        let elements = t.elements.mapIt(dump(it, 0)).join(", ")
        fmt"({elements})"
      of T.GenericVar:
        fmt"generic {t.label}"
      of T.Macro:
        fmt"macro {t.label}"
      of T.Any:
        fmt"any"
  result = "$1$2" % [offset, left]

iterator items*(t: Type): Type =
  case t.kind:
  of T.Simple:
    discard
  of T.Method:
    for arg in t.args:
      yield arg
    yield t.returnType
  of T.MethodOverload:
    for overload in t.overloads:
      yield overload
  of T.Compound:
    for arg in t.args:
      yield arg
  of T.Generic:
    discard
  of T.Object:
    for label, member in t.fields:
      yield member
  of T.Tuple, T.Union:
    for element in t.elements:
      yield element
  of T.GenericVar:
    discard
  of T.Macro:
    for arg in t.macroArgs:
      yield arg
  of T.Any:
    discard

proc `$`*(t: Type): string =
  result = dump(t, 0)

proc `[]`*(t: Type, args: varargs[Type]): Type =
  if t.kind != T.Generic:
    raise newException(Python2NimError, fmt"{$t.kind} []")
  else:
    result = Type(kind: T.Compound, args: @args, original: t)

proc unify*(a: Type, b: Type, genericMap: var Table[string, Type]): bool =
  # resolves generic vars
  if a.isNil or b.isNil:
    return false
  elif a.kind == T.GenericVar:
    if genericMap.hasKey(a.label):
      return genericMap[a.label] == b:
    else:
      genericMap[a.label] = b
      return true
  elif a.kind == T.MethodOverload and b.kind == T.Method:
    return a.overloads.anyIt(it.unify(b, genericMap))
  elif a.kind == T.Method and b.kind == T.MethodOverload:
    return b.overloads.anyIt(a.unify(it, genericMap))
  elif a.kind == T.Generic and b.kind == T.Compound:
    return a == b.original and zip(a.genericArgs, b.args).allIt(Type(kind: T.GenericVar, label: it[0]).unify(it[1], genericMap))
  elif a.kind != b.kind:
    return false
  else:
    case a.kind:
    of T.Simple:
      return a.label == b.label
    of T.Method:
      return len(a.args) == len(b.args) and zip(a.args, b.args).allIt(it[0].unify(it[1], genericMap)) and a.returnType.unify(b.returnType, genericMap)
    of T.MethodOverload:
      for aOverload in a.overloads:
        for bOverload in b.overloads:
          if aOverload.unify(bOverload, genericMap):
            return true
      return false
    of T.Compound:
      return a.original == b.original and zip(a.args, b.args).allIt(it[0].unify(it[1], genericMap))
    of T.Generic:
      return a.label == b.label and len(a.genericArgs) == len(b.genericArgs)
    of T.Object:
      return a.label == b.label
    of T.Tuple, T.Union:
      return len(a.elements) == len(b.elements) and zip(a.elements, b.elements).allIt(it[0].unify(it[1], genericMap))
    of T.GenericVar:
      return a.label == b.label
    of T.Macro:
      return false
    of T.Any:
      return true

proc deepCopy*(a: Type): Type =
  if a.isNil:
    return a
  result = genKind(Type, a.kind)
  result.label = a.label
  result.fullLabel = a.fullLabel
  case a.kind:
    of T.Method:
      result.args = a.args.mapIt(deepCopy(it))
      result.returnType = deepCopy(a.returnType)
    of T.MethodOverload:
      result.overloads = a.overloads.mapIt(deepCopy(it))
    of T.Compound:
      result.args = a.args.mapIt(deepCopy(it))
      result.original = deepCopy(a.original)
    of T.Generic:
      result.genericArgs = a.genericArgs.mapIt($it)
    of T.Object:
      result.base = deepCopy(a.base)
      result.inherited = a.inherited
      result.init = a.init
      result.fields = initTable[string, Type]()
      for label, typ in a.fields:
        result.fields[label] = deepCopy(typ)
    of T.Tuple, T.Union:
      result.elements = a.elements.mapIt(deepCopy(it))
    of T.Macro:
      result.macroArgs = a.macroArgs.mapIt(deepCopy(it))
    else:
      discard

proc get*(e: Env, name: string): Type

proc `[]`*(e: Env, name: string): Type =
  result = get(e, name)
  if result.isNil:
    raise newException(Python2NimError, "undefined $1" % name)

proc `[]=`*(e: Env, name: string, typ: Type) =
  e.types[name] = typ

proc hasKey*(e: Env, name: string): bool =
  not e.get(name).isNil

proc get*(e: Env, name: string): Type =
  var last = e
  while not last.isNil:
    if last.types.hasKey(name):
      return last.types[name]
    last = last.parent
  result = nil

proc childEnv*(e: Env, label: string, args: Table[string, Type], returnType: Type): Env =
  var argsChild = initTable[string, bool]()
  for arg, typ in args:
    argsChild[arg] = false
  result = Env(types: args, args: argsChild, returnType: returnType, label: label, parent: e)
  result.top = if e.isNil: result else: e.top


proc dump*(node: Node, depth: int, typ: bool = false): string =
  if node.isNil:
    return "nil"
  let offset = repeat("  ", depth)
  var left = ""
  let kind = $node.kind
  var typDump = if typ: "#$1" % dump(node.typ, 0) else: ""
  if typDump == "#nil":
    typDump = ""
  if left == "":
    left = case node.kind:
      of Int, PyInt:
        "Int($1)$2" % [$node.i, typDump]
      of Variable, PyOperator:
        $node.kind & "($1)$2" % [node.label, typDump]
      of PyStr:
        "PyStr($1)$2" % [node.text, typDump]
      of NodeMethod:
        "Method($1)\n$2\n$3" % [node.label, node.args.mapIt(dump(it, 0, typ)).join(" "), node.code.mapIt(dump(it, depth + 1, typ)).join("\n")]
      of Class:
        "Class($1)\n$2" % [node.label, node.methods.mapIt(dump(it.node, depth + 1, typ)).join(" ")]
      else:
        "$1$2:\n$3" % [kind, typDump, node.children.mapIt(dump(it, depth + 1, typ)).join("\n")]
  result = "$1$2" % [offset, left]

proc dumpList*(nodes: seq[Node], depth: int): string =
  result = nodes.mapIt(dump(it, depth, true)).join("\n")

const RUBY_LITERALS = {Int, Variable}

proc `[]`*(node: Node, index: int): var Node =
  case node.kind:
  of RUBY_LITERALS:
    raise newException(ValueError, "no index")
  else:
    return node.children[index]

proc `[]=`*(node: var Node, index: int, a: Node) =
  case node.kind:
  of RUBY_LITERALS:
    raise newException(ValueError, "no index")
  else:
    node.children[index] = a

iterator items*(node: Node): Node =
  case node.kind:
  of RUBY_LITERALS:
    discard
  else:
    for child in node.children:
      yield child

iterator mitems*(node: Node): var Node =
  for child in node.children.mitems:
    yield child

iterator nitems*(node: Node): (int, var Node) =
  var z = 0
  for child in node.children.mitems:
    yield (z, child)
    z += 1

proc `$`*(node: Node): string =
  result = dump(node, 0)


# proc notExpr*(node: Node): Node =
#   result = node
#   while result.kind == PyExpr:
#     result = result.children[0]

# proc testEq*(a: Node, b: Node): bool =
#   if a.isNil or b.isNil:
#     return false
#   elif a.kind == PyExpr or b.kind == PyExpr:
#     var newA = notExpr(a)
#     var newB = notExpr(b)
#     result = testEq(newA, newB)
#   elif a.kind != b.kind:
#     return false
#   else:
#     case a.kind:
#       of PyStr, PyBytes:
#       of PyInt:
#         result = a.i == b.i
#       of PyFloat:
#         result = a.f == b.f
#       of PyLabel, PyOperator:
#         result = a.label == b.label
#       of PyChar:
#         result = a.c == b.c
#       of PyHugeInt:
#         result = a.h == b.h
#       else:
#         if len(a.children) > 0 and len(b.children) > 0:
#           if len(a.children) != len(b.children):
#             return false
#           result = zip(a.children, b.children).allIt(it[0].testEq(it[1]))
#         else:
#           result = len(a.children) == 0 and len(b.children) == 0

proc deepCopy*(a: Node): Node =
  if a.isNil:
    return nil
  result = genKind(Node, a.kind)
  case a.kind:
  of PyStr, PyBytes:
    result.text = a.text
  of PyInt:
    result.i = a.i
  of PyFloat:
    result.f = a.f
  of PyLabel, Variable, PyOperator:
    result.label = a.label
  of PyHugeInt:
    result.h = a.h
  of PyChar:
    result.c = a.c
  of PyAssign:
    result.declaration = a.declaration
  of PyImport:
    result.aliases = a.aliases.mapIt(deepCopy(it))
  of PyFunctionDef:
    result.isIterator = a.isIterator
    result.isMethod = a.isMethod
    result.calls = a.calls
    result.isGeneric = a.isGeneric
  of Class:
    result.methods = a.methods.mapIt(Field(label: it.label, node: deepCopy(it.node)))
    result.label = a.label

  of NodeMethod:
    result.args = a.args.mapIt(deepCopy(it))
    result.code = a.code.mapIt(deepCopy(it))
    result.label = a.label
  else:
    discard
  result.children = @[]
  for child in a.children:
    result.children.add(deepCopy(child))
  result.typ = a.typ

proc camelCase*(label: string): string =
  var remainder = label
  var underline = 0
  while underline < len(remainder) and remainder[underline] == '_':
    underline += 1
  if underline > 0:
    remainder = remainder[underline..^1]
  var tokens = remainder.split("_")
  result = repeat("_", underline) & tokens[0] & tokens[1..^1].mapIt(if len(it) > 0: capitalizeAscii(it) else: "_").join("")

proc translateIdentifier*(label: string): string =
  result = camelCase(label)

proc translateIdentifier*(label: string, identifierCollisions: HashSet[string]): string =
  var translated = translateIdentifier(label)
  if translated notin identifierCollisions:
    return translated
  else:
    return label

proc load*(dbfile: string): TraceDB =
  new(result)
  result.root = parseJson(readFile(dbfile))
  result.types = initTable[string, Type]()
  result.sysPath = @[]
  result.modules = @[]
  result.projectDir = result.root{"@projectDir"}.getStr()
  result.package = result.projectDir.rsplit("/", 1)[1]
  for label, trace in result.root:
    if label == "@types":
      for childLabel, child in trace:
        if childLabel != "@path":
          # echo childLabel
          var typ: Type # = toType(importType(child))
          typ.fullLabel = childLabel
          if typ.kind == T.MethodOverload:
            for overload in typ.overloads.mitems:
              overload.fullLabel = childLabel
          result.types[childLabel] = typ
        else:
          result.sysPath = child.mapIt(($it)[1..^2])
    else:
      result.modules.add(label)


proc loadAst*(db: TraceDB, filename: string): Node =
  # XXX: Perhaps, this could be cached
  # echo filename
  # result = importAst(db.root[filename]["ast"])
  discard

proc startPath*(db: TraceDB): string =
  # TODO: smarter
  result = ""
  var maybeResult = ""
  for module in db.modules:
    if module.startsWith(db.projectDir):
      if module.endsWith("constants.py"):
        result = module
        break
      elif maybeResult == "":
        maybeResult = module
  if result == "":
    result = maybeResult


type
  RewriteRule* = ref object
    input*:   Node
    output*:  proc(node: Node, args: Table[string, Node], blockNode: Node, rule: RewriteRule): Node
    args*:    seq[seq[int]]
    replaced*: seq[tuple[label: string, typ: Type]]
    isGeneric*: bool

  Rewrite* = object
    rules*: seq[RewriteRule]

proc accepts(l: Type, r: Type): bool =
  if l.kind == T.Any:
    return true
  else:
    return l == r    

proc find(l: Node, r: Node): bool =
  if l.kind == Variable:
    if not l.typ.isNil and not l.typ.accepts(r.typ):
      return false
    else:
      return true
  if l.kind != r.kind:
    return false
  case l.kind:
  of Variable:
    result = l.label == r.label
  of PyStr, PyBytes:
    result = l.text == r.text
  of Int, PyInt:
    result = l.i == r.i
  of PyFloat:
    result = l.f == r.f
  of PyChar:
    result = l.c == r.c
  else:
    result = true
  if not result:
    return
  if l.children.len != r.children.len:
    return false
  for i in 0 .. < l.children.len:
    if not l.children[i].find(r.children[i]):
      return false
  result = true
  
proc find(rewrite: Rewrite, node: Node): seq[RewriteRule] =
  result = @[]
  # dump rewrite.rules.len
  for rule in rewrite.rules:
    if rule.input.find(node):
      # dump rule.input
      # dump node
      result.add(rule)

proc replace(rule: RewriteRule, node: Node, blockNode: Node): Node =
  var args = initTable[string, Node]()
  # dump node
  for i in 0 ..< rule.replaced.len:
    let r = rule.replaced[i].label
    let arg = rule.args[i]
    var k = 0
    var subNode = node
    while k < arg.len:
      subNode = subNode.children[rule.args[i][k]]
      k += 1
    args[r] = subNode
  # dump args
  result = rule.output(node, args, blockNode, rule)
  # dump rule.args
  result.isFinished = true

proc rewriteChildren(node: Node, rewrite: Rewrite, blockNode: Node): Node

proc rewriteNode(node: Node, rewrite: Rewrite, blockNode: Node): Node =
  var b: seq[RewriteRule] = @[]
  # dump node
  if not node.isFinished:
    b = rewrite.find(node)
  var newNode = node
  if b.len > 0:
    var c = b[0]
    for a in b:
      if c.isGeneric and not a.isGeneric:
        c = a
    newNode = c.replace(node, blockNode)
  return rewriteChildren(newNode, rewrite, blockNode)


proc rewriteChildren(node: Node, rewrite: Rewrite, blockNode: Node): Node =
  var newNode = deepCopy(node)
  if node.kind == Class:
    for i, met in node.methods:
      newNode.methods[i].node = rewriteNode(met.node, rewrite, blockNode)

  elif node.kind == NodeMethod:
    for i, child in node.code:
      newNode.code[i] = rewriteNode(child, rewrite, blockNode)

    for i, child in node.children:
      newNode.children[i] = rewriteNode(child, rewrite, blockNode)
  else:
    for i, child in node.children:
      newNode.children[i] = rewriteNode(child, rewrite, blockNode)
  return newNode

var inRuby {.compileTime.} = true

proc compileNode(node: NimNode, replaced: seq[(string, NimNode)]): NimNode =
  case node.kind
  of nnkCharLit..nnkUInt64Lit:
    return node
  of nnkFloatLit..nnkFloat128Lit:
    return node
  of nnkStrLit..nnkTripleStrLit:
    return node
  of nnkSym:
    return nnkCall.newTree(ident"variable", newLit($node))
  of nnkIdent:
    let label = $node
    var typ = newNilLit()
    for e in replaced:
      if label == e[0]:
        typ = e[1]
    return nnkCall.newTree(ident"variable", newLit($node), typ)
  of nnkStmtList:
    return compileNode(node[0], replaced)
  else:
    var sons = node.mapIt(compileNode(it, replaced))
    var call = ""
    case node.kind:
    of nnkCall, nnkCommand:
      call = "call"
      if node[0].kind == nnkDotExpr:
        call = "send"
        var oldSons = sons
        sons = @[]
        sons.add(oldSons[0][1])
        sons.add(oldSons[0][2])
        sons = sons.concat(oldSons[1 .. ^1])
        if inRuby and ($sons[1]).startsWith("is_"):
          sons[1] = newLit(($(sons[1]))[3 .. ^1] & "?")
    of nnkDotExpr:
      call = "attribute"
      sons[1] = sons[1][1]
    else:
      call = "unknown"
    result = nnkCall.newTree(ident(call))
    for son in sons:
      result.add(son)
    return result

include ast_dsl

proc args(node: NimNode, arg: string, a: var seq[int]): bool =
  case node.kind:
  of nnkCall:
    if node[0].kind == nnkIdent and $node[0] == "variable" and $node[1] == arg:
      return true
    else:
      for i, child in node:
        let iArg = i - 1
        if iArg == -1:
          continue
        
        a.add(i - 1)
        let res = args(child, arg, a)
        if res:
          return true
        else:
          discard a.pop()
      return false
  else:
    return false


proc args(node: NimNode, replaced: seq[(string, NimNode)]): seq[seq[int]] =
  result = @[]
  for arg in replaced:
    result.add(@[])
    discard args(node, arg[0], result[^1])
    
proc generateInput(input: NimNode): NimNode =
  let args = input[3]
  var help = ident("help")
  result = quote:
    var `help` = RewriteRule(input: nil, output: nil, replaced: @[], isGeneric: false)
  result = nnkStmtList.newTree(result)
  var replaced2: seq[(string, NimNode)]

  for i, arg in args:
    if i != 0:
      let label = newLit($arg[0])
      let typ = ident($arg[1] & "Type")
      var n = quote:
        `help`.replaced.add((
          `label`,
          `typ`))
      result.add(n)
      replaced2.add(($label, typ))

  let h = compileNode(input[^1], replaced2)
  var n = quote:
    `help`.input = `h`
  result.add(n)
  let args2 = args(h, replaced2)
  # dump args2
  # dump h.repr
  n = quote:
    @[]
  for argList in args2:
    var m = quote:
      @[]
    for arg in argList:
      m[1].add(newLit(arg))
    n[1].add(m)
  n = quote:
    `help`.args = `n`
  result.add(n)
  n = quote:
    rewriteList.rules.add(`help`)
  result.add(n)

var IntType = Type(kind: T.Simple, label: "Int")
var BoolType = Type(kind: T.Simple, label: "Bool")
var AnyType = Type(kind: T.Any)
var VoidType = Type(kind: T.Simple, label: "Void")

macro rewrite*(input: untyped, output: untyped): untyped =
  let inputNode = generateInput(input)
  assert output.kind == nnkStmtList and output[0][0].repr == "interlang"
  result = inputNode
  let help = ident("help")
  let code = output[0][1]
  let outputCall = nnkLambda.newTree(
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      newIdentNode("Node"),
      nnkIdentDefs.newTree(
        newIdentNode("node"),
        newIdentNode("Node"),
        newEmptyNode()
      ),
      nnkIdentDefs.newTree(
        newIdentNode("args"),
        nnkBracketExpr.newTree(
          newIdentNode("Table"),
          newIdentNode("string"),
          newIdentNode("Node")
        ),
        newEmptyNode()
      ),
      nnkIdentDefs.newTree(
        newIdentNode("blockNode"),
        newIdentNode("Node"),
        newEmptyNode()
      ),
      nnkIdentDefs.newTree(
        newIdentNode("rule"),
        newIdentNode("RewriteRule"),
        newEmptyNode()
      )
    ),
    newEmptyNode(),
    newEmptyNode(),
    code
  )


  var n = quote:
    `help`.output = `outputCall`
  result.add(n)
  result = quote:
    block:
      `result`
  # dump result.repr

var rewriteList = Rewrite(rules: @[])

rewrite do (x: Int):
  x.is_positive()
do:
  interlang:
    send(args["x"], "is_positive", BoolType)
    
rewrite do (x: Any):
  puts x
do:
  interlang:
    call(variable("echo"), args["x"], VoidType)

var rewriteinputruby = rewriteList
rewriteList = Rewrite(rules: @[])

static:
  inRuby = false

rewrite do (x: Int):
  x.is_positive()
do:
  interlang:
    compare(operator(">"), args["x"], 0, BoolType)

rewrite do (x: Any):
  echo(x)
do:
  interlang:
    call(variable("echo"), args["x"], VoidType)

var rewritenim = rewriteList

var A = Type(kind: T.Object, label: "A", fields: initTable[string, Type]())


var input = Node(
  kind: Class,
  label: "A",
  docstring: @[],
  fields: @[],
  methods: @[
    Field(
      label: "b",
      node: Node(
        kind: NodeMethod,
        label: "b",
        typ: Type(kind: T.Method, args: @[A, IntType], returnType: VoidType),
        # (arg) # self int
        args: @[
          input_variable("self"),
          input_variable("arg", IntType)
        ],
        # puts arg.positive?
        code: @[
          input_call(
            input_variable("puts"),
            @[
              input_send(
                input_variable("arg"),
                "positive?",
                BoolType)])]))],
  typ: A)

proc analyze(node: Node, env: Env) =
  case node.kind:
  of Class:
    for met in node.methods.mitems:
      analyze(met.node, env)
  of NodeMethod:
    var args = initTable[string, Type]()
    for arg in node.args:
      args[arg.label] = arg.typ
    var met = childEnv(env, node.label, args, node.returnType)
    for subNode in node.code:
      analyze(subNode, met)
  of Send:
    analyze(node.children[0], env)
    for arg in node.children[2 .. ^1]:
      analyze(arg, env)
  of Call:
    analyze(node.children[0], env)
    for arg in node.children[1 .. ^1]:
      analyze(arg, env)
  of Variable:
    node.typ = env.get(node.label)
  of Int:
    node.typ = IntType
  of Assign:
    analyze(node.children[1], env)
    node.children[0].typ = node.children[1].typ
    if node.children[0].kind == Variable:
      env[node.children[0].label] = node.children[0].typ
  of PyStr, PyBytes:
    discard
  of PyInt, PyFloat, PyChar, PyHugeInt, PyAssign, PyFunctionDef, PyClassDef:
    discard
  else:
    for child in node.children:
      analyze(child, env)




var env = Env(parent: nil, types: initTable[string, Type]())

input.analyze(env)

proc rewriteProgram(node: Node, rewrite: Rewrite): Node =
  case node.kind:
  of Class:
    result = node.deepCopy()
    for i, met in node.methods:
      var newCode: seq[Node] = @[]
      for element in met.node.code:
        newCode.add(rewriteNode(element, rewrite, met.node))
      result.methods[i].node.code = newCode
  else:
    discard

echo "after analyze ", dump(input, 0, true)
input = rewriteProgram(input, rewriteinputruby)
# two directions
echo "after ruby ", dump(input, 0, true)
input = rewriteProgram(input, rewritenim)
# echo "after rewrite ", dump(input, 0, true)

include generator

var generator = Generator(indent: 2, v: V019, module: Module(), identifierCollisions: initSet[string]())
var nimNode = generator.generateClass(input)
echo nimNode.renderTree({renderDocComments}) & "\n"
