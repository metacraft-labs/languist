# those type nodes are closer to nim's type system
# they are easily mappable to PNode

import sequtils, strutils, strformat, tables, sugar, hashes, errors, gen_kind, sets, json

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
      members*:   Table[string, Type]
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
    Class, NodeMethod, Call, Variable,
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
    idiomatic*: bool # Makes sure a node is converted to an idiom max 1
    line*: int # Line, -1 or actual
    column*: int # Column, -1 or actual
    ready*: bool # Ready for gen
    label*: string
    case kind*: NodeKind:
    of PyStr, PyBytes:
      text*: string
    of PyInt:
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
      isIterator*: bool
      isMethod*: bool
      calls*: HashSet[string]
      isGeneric*: bool
      doc*: seq[string]
    of PyClassDef:
      docstring*: seq[string]
    of Class:
      fields*: seq[Field]
      methods*: seq[Field]
    of NodeMethod:
      id*: string
      returnType*: Type
      args*: seq[Node]
      code*: seq[Node]
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
        for label, member in t.members:
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
    for label, member in t.members:
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
      result.members = initTable[string, Type]()
      for label, typ in a.members:
        result.members[label] = deepCopy(typ)
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

proc `[]=`*(e: var Env, name: string, typ: Type) =
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

