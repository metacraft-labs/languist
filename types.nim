
import sequtils, strutils, strformat, tables, sugar, hashes, gen_kind, sets, json, macros, terminal, helpers, os
import
  compiler/[ast]

type
  T* = enum 
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
    fieldLabel*: string
    rewritten*: bool
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
    root*:        JsonNode
    paths*:       seq[string]
    modules*:     seq[Module]
    types*:       Table[string, Type]
    sysPath*:     seq[string]
    projectDir*:  string
    package*:     string
    rewrite*:     Rewrite
    targetFolder*: string
    config*:      Config
    methods*:     Table[string, Type]
    lang*:        Lang
  
  Env* = ref object
    types*:       Table[string, Type]
    args*:        Table[string, bool]
    returnType*:  Type
    label*:       string
    parent*:      Env
    top*:         Env
    hasYield*:    bool
    declarations*: Table[string, (Type, Declaration)]

  NodeKind* = enum
    Class, NodeMethod, Call, Variable, Int, Send, Assign, Attribute, String, Bool, New, Nil, Float, Code, While, Import, Return, Block, ForRange, Self, If, Raw, Operator, BinOp, Char, Sequence, NimTable, Symbol, Pair, UnaryOp, Break, Yield, Index, Continue, NimSlice, ForIn, Docstring, Command, MacroCall, Try, Except, Raise, Empty, Case, Of, Range, Super, Comment,
    AugOp, ImportFrom,
    RubyConst, RubyMasgn, RubyMlhs, RubySplat, RubyIRange, RubyRegexp, RubyRegopt, 
    RubyAlias, 
    NimWhen, NimRange, NimRangeLess, NimCommentedOut, NimExprColonExpr, NimInfix, NimAccQuoted, NimOf, NimPrefix, NimIf, NimElif, NimElse, NimTuple

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
    genBlock*: bool
    case kind*: NodeKind:
    of String, Symbol, Docstring, Comment:
      text*: string
    of Int:
      i*: int
    of Float:
      f*: float
    of Char:
      c*: char
    of Bool:
      val*: bool
    of Assign:
      declaration*: Declaration
    of Import:
      aliases*: seq[Node]
    # of PyFunctionDef:
    #   calls*: HashSet[string]
    of Class:
      fields*: seq[Field]
      methods*: seq[Field]
    of NodeMethod, Block:
      # id*: string
      isIterator*: bool
      isMethod*: bool
      isGeneric*: bool
      isClass*: bool
      returnType*: Type
      args*: seq[Node]
      code*: seq[Node]
    else:
      discard
    docstring*: seq[string]
    children*: seq[Node] # complicates everything to have it disabled for several nodes

  Field* = object
    label*: string
    node*: Node

  Module* = ref object
    name*: string
    path*: string
    imports*: seq[Node] # probably Import and Assign
    types*: seq[Node] # probably ClassDef
    classes*: seq[Node] # probably FunctionDef
    main*: seq[Node] # other top level stuff
  
  TypeDependency* = object
    methods*: Table[string, seq[string]]
    ignore*:  seq[string]
    all*:     seq[string]

  Python2NimError* = object of Exception

  NimVersion* = enum V019, Development

  Generator* = ref object
    indent*:               int
    v*:                    NimVersion
    module*:               Module
    identifierCollisions*: HashSet[string]
    types*:                  PNode
    global*:                 PNode
    methods*:                PNode
    main*:                   PNode
    top*:                    PNode
    config*:                 Config
    lang*:                   Lang

  RewriteRule* = ref object
    input*:   Node
    output*:  proc(node: Node, args: Table[string, Node], blockNode: Node, rule: RewriteRule): Node
    args*:    seq[seq[int]]
    replaced*: seq[tuple[label: string, typ: Type]]
    isGeneric*: bool
    dependencies*: seq[string]

  Rewrite* = ref object
    rules*: seq[RewriteRule]
    types*: Table[string, Type]
    genBlock*: seq[string]
    symbolRules*: seq[SymbolRule]
    lastCalls*: seq[SymbolRule]


  SymbolRule* = ref object
    label*: string
    elements*: seq[string]
    handler*: proc(a: string): Node

  Config* = object
    indent*: int
    imports*: seq[string]
    name*: string
    ignoreMethods*: seq[string]

  Lang* = enum Ruby, Python

let endl* = "\n"

var debug* = false #true

template eecho*(a: untyped): untyped =
  if debug:
    echo a

template edump*(a: untyped): untyped =
  if debug:
    dump a

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
  result = Env(types: args, args: argsChild, returnType: returnType, label: label, parent: e, declarations: initTable[string, (Type, Declaration)]())
  result.top = if e.isNil: result else: e.top

proc dump*(node: Node, depth: int, typ: bool = false): string =
  if node.isNil:
    return "nil"
  let offset = repeat("  ", depth)
  let kind = $node.kind
  var typDump = if typ: "#$1" % dump(node.typ, 0) else: ""
  if typDump == "#nil":
    typDump = ""
  var left = case node.kind:
    of Int:
      "Int($1)$2" % [$node.i, typDump]
    of Bool:
      "Bool($1)$2" % [$node.val, typDump]
    of Variable, Operator, RubyConst:
      $node.kind & "($1)$2" % [node.label, typDump]
    of String, Symbol, Docstring, Comment:
      $node.kind & "($1)$2" % [node.text, typDump]
    of NodeMethod, Block:
      if node.typ.isNil:
        eecho "BLOCK NIL"
      $node.kind & "($1)\n$2\n$3" % [node.label, node.args.mapIt(dump(it, 0, typ)).join(" "), node.code.mapIt(dump(it, depth + 1, typ)).join("\n")]
    of Class:
      "Class($1, $2)\n$3" % [node.label, dump(node.typ.base, 0), node.methods.mapIt(dump(it.node, depth + 1, typ)).join(" ")]
    else:
      var temp = "$1$2:\n$3" % [kind, typDump, node.children.mapIt(dump(it, depth + 1, typ)).join("\n")]
      if node.kind == Assign:
        temp.add("\n" & offset & "  " & $node.declaration)
      temp
  result = "$1$2" % [offset, left]

proc dump*(m: Module, depth: int, typ: bool = false): string =
  "$1\n$2" % [m.classes.mapIt(dump(it, depth, typ)).join("\n"), m.main.mapIt(dump(it, depth, typ)).join("\n")]
    
proc dumpList*(nodes: seq[Node], depth: int): string =
  result = nodes.mapIt(dump(it, depth, true)).join("\n")

var IntType* = Type(kind: T.Simple, label: "Int")
var BoolType* = Type(kind: T.Simple, label: "Bool")
var AnyType* = Type(kind: T.Any)
var VoidType* = Type(kind: T.Simple, label: "Void")
var StringType* = Type(kind: T.Simple, label: "String")
var MethodType* = Type(kind: T.Method)
var SequenceType*     = Type(kind: T.Generic, label: "Sequence", genericArgs: @["T"])
var TableType*     = Type(kind: T.Generic, label: "Table", genericArgs: @["K", "V"])
var SymbolType* = Type(kind: T.Simple, label: "Symbol")
var ClassType* = Type(kind: T.Simple, label: "Class")

proc sequenceType*(element: Type): Type =
  result = Type(kind: T.Compound, args: @[element], original: SequenceType)

proc tableType*(key: Type, value: Type): Type =
  result = Type(kind: T.Compound, args: @[key, value], original: TableType)

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


proc notExpr*(node: Node): Node =
  result = node
  # while result.kind == PyExpr:
  #   result = result.children[0]

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
  of String, Docstring, Comment:
    result.text = a.text
    if a.kind != Comment:
      result.typ = StringType
  of Int:
    result.i = a.i
    result.typ = IntType
  of Bool:
    result.val = a.val
    result.typ = BoolType
  of Variable, Operator, RubyConst:
    result.label = a.label
  # of PyHugeInt:
  #   result.h = a.h
  of Char:
    result.c = a.c
  of Assign:
    result.declaration = a.declaration
  of Import:
    result.aliases = a.aliases.mapIt(deepCopy(it))
  of Class:
    result.methods = a.methods.mapIt(Field(label: it.label, node: deepCopy(it.node)))
    result.label = a.label
    result.docstring = a.docstring
    result.typ = deepCopy(a.typ)
  of Symbol:
    result.text = a.text
    result.typ = SymbolType
  of NodeMethod, Block:
    result.args = a.args.mapIt(deepCopy(it))
    result.code = a.code.mapIt(deepCopy(it))
    result.label = a.label
    result.isMethod = a.isMethod
    result.isIterator = a.isIterator
    result.returnType = deepCopy(a.returnType)
    result.docstring = a.docstring

  else:
    discard
  # echo a
  result.children = @[]
  for child in a.children:
    result.children.add(deepCopy(child))
  result.typ = deepCopy(a.typ)

proc getLang*(name: string): Lang =
  case name:
  of ".py":
    Lang.Python
  of ".rb":
    Lang.Ruby
  else:
    Lang.Ruby

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
  var l = label
  if l.endsWith("?"):
    l = "is_" & l[0 .. ^2]
  result = camelCase(l)
  

proc translateIdentifier*(label: string, identifierCollisions: HashSet[string]): string =
  var translated = translateIdentifier(label)
  if translated notin identifierCollisions:
    return translated
  else:
    return label

