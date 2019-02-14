# A system similar to pseudo api translators

# Python2Nim

import sequtils, strutils, macros, tables
import algorithm, strformat, ../core, ../python_ast, ../nim_types, ../errors, ../dependency

type
  IdiomKind* = enum IType, IMethod, IFunction, IAST

  Idiom* = ref object
    case kind*: IdiomKind:
    of IType:
      typ*:       Type
    of IMethod, IFunction:
      receiver*:  Node # nil IFunction
      name*:      string
      args*:      seq[Node]
    of IAST:
      handler*:   proc(nodes: seq[Node]): Node

  OperatorIdiom* = object
    left*:      Type
    right*:     Type
    idiom*:     Idiom

  MethodIdiom* = object
    receiver*:    Type
    genericArgs*: seq[Type]
    args*:        seq[Type]
    argNames*:    seq[string]
    idiom*:     Idiom

  IdiomError* = object of Python2NimError

var operatorTypes* = initTable[string, seq[OperatorIdiom]]()
var builtinMethods* = initTable[Type, Table[string, seq[MethodIdiom]]]()
var requiredDependencies* = initTable[Type, TypeDependency]()

proc `$`*(i: Idiom): string =
  result = case i.kind:
    of IType:
      fmt"Idiom Type: {$i.typ}"
    of IMethod:
      fmt"Idiom Method: {$i.receiver} {i.name}({$len(i.args)})"
    of IFunction:
      fmt"Idiom Function: {i.name}({$len(i.args)})"
    of IAST:
      fmt"Idiom AST"

proc generateOperators(types: NimNode): NimNode =
  result = nnkStmtList.newTree()
  let handlerNode = ident("handler")
  var handler = quote:
    var `handlerNode`: proc(nodes: seq[Node]): Node
  result.add(handler)
  for t in types:
    if t.kind == nnkInfix and $(t[0]) == "->":
      assert t[1].kind == nnkCall and t[1][0].kind == nnkAccQuoted
      let op = newLit($t[1][0][0])
      assert len(t[1]) == 3
      let left = t[1][1]
      let right = t[1][2]
      let ret = t[2]
      let last = quote:
        if not operatorTypes.hasKey(`op`):
          operatorTypes[`op`] = @[]
        operatorTypes[`op`].add(OperatorIdiom(left: `left`, right: `right`, idiom: Idiom(kind: IType, typ: `ret`)))
      result.add(last)
    elif t.kind == nnkCall and t[0].kind == nnkAccQuoted:
      assert len(t) == 4
      let op = newLit($t[0][0])
      let left = t[1]
      let right = t[2]
      let ast = t[3]
      let leftNode = ident("left")
      let rightNode = ident("right")
      handler = quote:
        `handlerNode` = proc(nodes: seq[Node]): Node =
          let `leftNode` = nodes[0]
          let `rightNode` = nodes[2]
          `ast`
      let last = quote:
        if not operatorTypes.hasKey(`op`):
          operatorTypes[`op`] = @[]
        operatorTypes[`op`].add(OperatorIdiom(
          left: `left`,
          right: `right`,
          idiom: Idiom(kind: IAST, handler: `handlerNode`)))
      result.add(handler)
      result.add(last)

proc markIdiomatic*(node: var Node) =
  node.idiomatic = true
  for child in node.mitems:
    child.markIdiomatic()

proc applyIdiom*(node: var Node, typ: Type, name: string, idiom: Idiom, children: seq[Node] = @[]): (Node, seq[string]) =
  var imports: seq[string] = @[]
  var newNode = case idiom.kind:
    of IType:
      node.typ = idiom.typ
      node
    of IMethod:
      node
    of IFunction:
      node
    of IAST:
      idiom.handler(if len(children) > 0: children else: node.children)
  if node.kind == PyBinOp and node[1].kind == PyPow:
    imports.add("math")
  if not typ.isNil:
    var baseType = if typ.kind == N.Compound: typ.original else: typ
    if requiredDependencies.hasKey(baseType):
      var dependencies = requiredDependencies[baseType]
      if name notin dependencies.ignore:
        imports = imports.concat(dependencies.all)
      if dependencies.methods.hasKey(name):
        imports = imports.concat(dependencies.methods[name])
  result = (newNode, imports)

proc initName(node: NimNode): (string, seq[Type]) =
  if node.kind == nnkIdent:
    result = ($node, @[])
  elif node.kind == nnkBracketExpr:
    var z = 0
    var name = ""
    var types: seq[Type] = @[]
    for child in node:
      if z == 0:
        name = $child
      else:
        types.add(Type(kind: N.GenericVar, label: $child))
      z += 1
    result = (name, types)

proc generateBuiltin*(u: NimNode, children: NimNode): NimNode =
  # count(sub: T.String) =>           receiver.count(sub): T.String
  # Infix
  # Ident ident"=>"
  # ObjConstr
  #   Ident ident"count"
  #   ExprColonExpr
  #     Ident ident"sub"
  #     DotExpr
  #       Ident ident"T"
  #       Ident ident"String"
  # Call
  #   DotExpr
  #     Ident ident"receiver"
  #     Ident ident"count"
  #   Ident ident"sub"
  # StmtList
  #   DotExpr
  #     Ident ident"T"
  #     Ident ident"String"

  # count(sub: T.String) =>           count: T.String
  # Infix
  # Ident ident"=>"
  # ObjConstr
  #   Ident ident"count"
  #   ExprColonExpr
  #     Ident ident"sub"
  #     DotExpr
  #       Ident ident"T"
  #       Ident ident"String"
  # Ident ident"count"
  # StmtList
  #   DotExpr
  #     Ident ident"T"
  #     Ident ident"String"

  # count(sub: T.String): call(attribute(receiver, "count"), @[sub], T.String)
  # Call
  # ObjConstr
  #   Ident ident"count"
  #   ExprColonExpr
  #     Ident ident"sub"
  #     DotExpr
  #       Ident ident"T"
  #       Ident ident"String"
  # StmtList
  #   Call
  #     Ident ident"call"
  #     Call
  #       Ident ident"attribute"
  #       Ident ident"receiver"
  #       StrLit count
  #     Prefix
  #       Ident ident"@"
  #       Bracket
  #         Ident ident"sub"
  #     DotExpr
  #       Ident ident"T"
  #       Ident ident"String"


  result = nnkStmtList.newTree()
  var genericVars: seq[Type] = @[]
  var genericNode = nnkStmtList.newTree()
  var typ = u
  if typ.kind == nnkBracketExpr:
    typ = u[0]
    for child in u:
      if child.kind != nnkDotExpr:
        genericVars.add(Type(kind: N.GenericVar, label: $child))
        let childNode = newLit($child)
        var g = quote:
          let `child` = Type(kind: N.GenericVar, label: `childNode`)
        genericNode.add(g)
  let methodIdiomNode = ident("methodIdiom")
  let idiomNode = ident("idiom")
  let handlerNode = ident("handler")
  let nodes = ident("nodes")
  let receiverNode = ident("receiver")
  var m = quote:
    var `methodIdiomNode` = MethodIdiom()
    var `idiomNode`: Idiom
    var `handlerNode`: proc (`nodes`: seq[Node]): Node
    `genericNode`
    builtinMethods[`typ`] = initTable[string, seq[MethodIdiom]]()
    requiredDependencies[`typ`] = TypeDependency(methods: initTable[string, seq[string]](), ignore: @[], all: @[])
  result.add(m)

  for child in children:
    var pythonName: NimNode
    var methodName = ""
    var args: seq[NimNode] = @[]
    var argNames: seq[NimNode] = @[]
    var q: NimNode
    var handlerStart: NimNode
    var handlerCode: NimNode
    var ret: NimNode
    var childGenericVars: seq[Type] = @[]

    handlerStart = quote:
      `handlerNode` = proc(`nodes`: seq[Node]): Node =
        let `receiverNode` = `nodes`[0]
    if child.kind == nnkInfix:
      assert child[0].kind == nnkIdent and $child[0] == "=>"
      # assert child[1].kind == nnkCall and child[1][0].kind == nnkIdent
      if child[1].kind == nnkObjConstr:
        (methodName, childGenericVars) = initName(child[1][0])
        childGenericVars = genericVars.concat(childGenericVars)
        pythonName = newLit(methodName)
        for arg in child[1]:
          if arg.kind == nnkExprColonExpr:
            argNames.add(newLit($arg[0]))
            args.add(arg[1])
      elif child[1].kind == nnkCall:
        (methodName, childGenericVars) = initName(child[1][0])
        childGenericVars = genericVars.concat(childGenericVars)
        pythonName = newLit(methodName)
        argNames = @[]
        args = @[]
      var receiver: NimNode
      var nimName: NimNode
      var nimArgs: seq[NimNode] = @[]
      ret = child[3][0]
      if child[2].kind == nnkCall:
        if child[2][0].kind == nnkDotExpr:
          receiver = child[2][0][0]
          nimName = newLit($child[2][0][1])
          for arg in child[2]:
            if arg.kind != nnkDotExpr:
              nimArgs.add(arg)
          handlerCode = quote:
            Node(
              kind: PyCall,
              children: @[
                Node(
                  kind: PyAttribute,
                  children: @[`receiver`, Node(kind: PyStr, s: `nimName`)]),
                Node(kind: Sequence, children: @[])],
              typ: `ret`)
        elif child[2][0].kind == nnkIdent:
          receiver = nil
          nimName = newLit($child[2][0])
          var isArg = false
          for arg in child[2]:
            if not isArg:
              isArg = true
            else:
              nimArgs.add(arg)
          handlerCode = quote:
            Node(
              kind: PyCall,
              children: @[
                Node(kind: PyLabel, label: `nimName`),
                Node(kind: Sequence, children: @[])],
              typ: `ret`)
        for arg in nimArgs:
          handlerCode[^2][^1][^1][^1][^1][^1][^1].add(arg)
      elif child[2].kind == nnkIdent:
        receiver = receiverNode
        nimName = newLit($child[2])
        nimArgs = argNames.mapIt(ident($it))
        handlerCode = quote:
          Node(
            kind: PyCall,
            children: @[
              Node(
                kind: PyAttribute,
                children: @[`receiver`, Node(kind: PyStr, s: `nimName`)]),
              Node(kind: Sequence, children: @[])],
          typ: `ret`)
        for arg in nimArgs:
          handlerCode[^2][^1][^1][^1][^1][^1][^1].add(arg)
    elif child.kind == nnkCall:
      if child[0].kind == nnkObjConstr:
        (methodName, childGenericVars) = initName(child[0][0])
        childGenericVars = genericVars.concat(childGenericVars)
        pythonName = newLit(methodName)
        for arg in child[0]:
          if arg.kind == nnkExprColonExpr:
            argNames.add(newLit($arg[0]))
            args.add(arg[1])
      elif child[0].kind == nnkCall:
        methodName = $child[0][0]
        pythonName = newLit(methodName)
        argNames = @[]
        args = @[]
      handlerCode = child[1]
    elif child.kind == nnkAsgn:
      let name = $child[0]
      let fields = {"dependencies": "methods", "dependenciesIgnore": "ignore", "dependenciesAll": "all"}.toTable()
      let field = ident(fields[name])
      let dependencies = child[1]
      q = quote:
        requiredDependencies[`typ`].`field` = `dependencies`
      result.add(q)
      continue
    let argNamesNode = nnkPrefix.newTree(ident("@"), nnkBracket.newTree(argNames))
    let argsNode = nnkPrefix.newTree(ident("@"), nnkBracket.newTree(args))
    var childGenericNode = nnkPrefix.newTree(ident("@"), nnkBracket.newTree())
    for v in childGenericVars:
      let v2 = newLit(v.label)
      q = quote:
        Type(kind: GenericVar, label: `v2`)
      childGenericNode[1].add(q)
    q = quote:
      `methodIdiomNode` = MethodIdiom(receiver: `typ`, genericArgs: `childGenericNode`, args: `argsNode`, argNames: `argNamesNode`, idiom: nil)
    result.add(q)
    for z, arg in argNames:
      let z2 = newLit(z + 1)
      let a = ident($arg)
      let q2 = quote:
        let `a` = `nodes`[`z2`]
      handlerStart[1][^1].add(q2)

    if not handlerCode.isNil:
      handlerStart[1][^1].add(handlerCode)
      result.add(handlerStart)
      q = quote:
        `idiomNode` = Idiom(kind: IAST, handler: `handlerNode`)
        `methodIdiomNode`.idiom = `idiomNode`
      result.add(q)

      let n = quote:
        if not builtinMethods[`typ`].hasKey(`pythonName`):
          builtinMethods[`typ`][`pythonName`] = @[]
        builtinMethods[`typ`][`pythonName`].add(`methodIdiomNode`)
      result.add(n)
  # echo result.repr

macro operators*(types: untyped): untyped =
  # a dsl
  # `op`(LeftType <Type>, RightType <Type>) -> Typ <Type>
  # `op`(LeftType <Type>, RightType <Type>):
  #    nim code producing ast based on `left` and `right` node
  result = generateOperators(types)

macro builtin*(typ: untyped, children: untyped): untyped =
  # methods: we have magical variables
  # receiver which is equivalent to the python object that receives the method
  # (bit smalltalk naming)
  # pymethod(argName: ArgType <Type>*) => methodName: <Type> we assume it's receiver.methodName(argName*)
  # pymethod(argName: ArgType <Type>*) => nimfunction(argName*): <Type>
  # pymethod(argName: ArgType <Type>*): handler
  # nimfunction can be <stuff>.<method>(<args>) or <function>(<args>)
  # dependencies = <a table> method: seq[lib]
  # dependenciesAll = seq[lib] when all methods need a lib
  # dependenciesIgnore = seq[lib] blacklist methods that doesnt need a lib
  result = generateBuiltin(typ, children)

# decorator("curry", "utils.nim", "curry") # when a curry decorator is used, apply custom macro

# system:
#   py("str"):
#     method("islower", "isLowerAscii", lib="strutils")

# # map methods and builtin functions

# map_module("functools"):
#   method("lru_cache") do (maxsize: int, typed: bool):
#     # map modules to nim
#     # we can reuse existing functions and types
#     # or we can generate ast/code that does the same thing in Nim

proc readOp*(node: Node): string =
  result = case node.kind:
    of PyAdd: "+"
    of PySub: "-"
    of PyMult: "*"
    of PyDiv: "/"
    of PyFloorDiv: "//"
    of PyPow: "**"
    of PyLShift: "<<"
    of PyRShift: ">>"
    of PyBitAnd: "&"
    of PyBitOr: "|"
    else: "?"

proc applyOperatorIdiom*(node: var Node, maybe: bool = false): (Node, seq[string]) =
  var idiom: Idiom
  let op = readOp(node.children[1])
  let left = node.children[0]
  let right = node.children[2]
  var imports: seq[string] = @[]

  if operatorTypes.hasKey(op):
    let operatorIdioms = operatorTypes[op]

    for operatorIdiom in operatorIdioms:
      if operatorIdiom.left == left.typ and operatorIdiom.right == right.typ:
        idiom = operatorIdiom.idiom
        break

    if idiom.isNil and left.typ == right.typ and left.typ.kind == N.Compound:
      for operatorIdiom in operatorIdioms:
        var genericMap = initTable[string, Type]()
        if left.typ.original.label == "seq":
          genericMap["T"] = left.typ.args[0]
          if len(imports) == 0:
            imports.add("sequtils")
        elif left.typ.original.label == "Table":
          genericMap["K"] = left.typ.args[0]
          genericMap["V"] = left.typ.args[1]
          if len(imports) == 0:
            imports.add("tables")
        if operatorIdiom.left.unify(left.typ, genericMap) and operatorIdiom.right.unify(right.typ, genericMap):
          idiom = operatorIdiom.idiom
          break

  if idiom.isNil:
    imports = @[]
    # if not maybe:
      # raise newException(IdiomError, fmt"idiom for {op}({left.typ}, {right.typ})")
    # else:
    result = (nil, imports)
    return

  result = applyIdiom(node, nil, "", idiom)
  result[1] = result[1].concat(imports)


proc replaceGeneric*(typ: Type, genericMap: Table[string, Type]): Type =
  if typ.isNil:
    return typ
  case typ.kind:
  of N.Atom:
    return typ
  of N.Function:
    var z = 0
    var newTyp = typ
    for arg in typ.functionArgs:
      newTyp.functionArgs[z] = arg.replaceGeneric(genericMap)
    newTyp.returnType = typ.returnType.replaceGeneric(genericMap)
    return newTyp
  of N.Overloads:
    var z = 0
    var newTyp = typ
    for overload in typ.overloads:
      newTyp.overloads[z] = overload.replaceGeneric(genericMap)
    return newTyp
  of N.Compound:
    var z = 0
    var newTyp = typ
    for arg in typ.args:
      newTyp.args[z] = arg.replaceGeneric(genericMap)
    return newTyp
  of N.Generic:
    return typ
  of N.Record:
    return typ
  of N.Tuple:
    return Type(kind: N.Tuple, elements: typ.elements.mapIt(it.replaceGeneric(genericMap)))
  of N.GenericVar:
    if genericMap.hasKey(typ.label):
      return genericMap[typ.label]
    else:
      return typ
  of N.Macro:
    return typ
  of N.Any:
    return typ

proc replaceGeneric*(node: var Node, genericMap: Table[string, Type]): Node =
  node.typ = node.typ.replaceGeneric(genericMap)
  var z = 0
  for child in node.mitems:
    node.children[z] = child.replaceGeneric(genericMap)
    z += 1
  return node

proc loadMethodIdiom*(node: var Node, receiver: Node, name: string, args: seq[Node]): (Idiom, Table[string, Type]) =
  var idiom: Idiom
  var genericMap = initTable[string, Type]()
  var typ: Type
  if receiver.isNil:
    typ = T.Void
  else:
    typ = receiver.typ
    if typ.isNil:
      result = (nil, genericMap)
      return
  if typ.kind == N.Compound:
    typ = typ.original

  if builtinMethods.hasKey(typ) and builtinMethods[typ].hasKey(name):
    let methodIdioms = builtinMethods[typ][name]

    for methodIdiom in methodIdioms:
      if len(methodIdiom.args) == len(args):
        if len(args) == 0:
          idiom = methodIdiom.idiom
          break
        genericMap = initTable[string, Type]()
        if receiver.typ.kind == N.Compound:
          for z in 0..<len(receiver.typ.args):
            genericMap[methodIdiom.genericArgs[z].label] = receiver.typ.args[z]
        var valid = true
        for z in 0..<len(args):
          if not methodIdiom.args[z].unify(args[z].typ, genericMap):
            valid = false
            break
        if valid:
          idiom = methodIdiom.idiom
          break
  result = (idiom, genericMap)

proc applyMethodIdiom*(node: var Node, receiver: Node, name: string, args: seq[Node]): (Node, seq[string]) =
  var (idiom, genericMap) = loadMethodIdiom(node, receiver, name, args)
  if idiom.isNil:
    raise newException(IdiomError, fmt"idiom for {$receiver.typ}.{name}({$len(args)})")

  var (n, imports) = applyIdiom(node, receiver.typ, name, idiom, @[receiver].concat(args))
  result = (n.replaceGeneric(genericMap), imports)

proc maybeApplyMethodIdiom*(node: var Node, receiver: Node, name: string, args: seq[Node]): (Node, seq[string]) =
  var (idiom, genericMap) = loadMethodIdiom(node, receiver, name, args)
  if idiom.isNil:
    result = (nil, @[])
  else:
    var (n, imports) = applyIdiom(node, receiver.typ, name, idiom, @[receiver].concat(args))
    result = (n.replaceGeneric(genericMap), imports)
