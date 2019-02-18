import
  strformat, os, strutils, sequtils, tables
  # types, core, env, tracer, sets,
  # ast_parser, ast_dsl, generator, module, helpers,
  # idioms/[idioms_dsl, operators, string_methods, list_methods], trace_db

# template log(a: Textable) =
#   if debug:
#     echo $a

# template log(a: string) =
#   if debug:
#     echo a

# proc newCompiler*(db: DeducktDb, command: string): Compiler =
#   result = Compiler(db: db, command: command)

# proc moduleOf*(compiler: Compiler, name: string): string =
#   let tokens = compiler.currentModule.split(".")
#   var m: seq[string] = @[]
#   for z, token in tokens:
#     if token == compiler.db.package:
#       m = tokens[z..^1]
#       break
#   m.add(name)
#   result = m.join(".")

# proc compileNode*(compiler: var Compiler, node: var Node, env: var Env): Node

# proc mergeFunctionTypeInfo(compiler: var Compiler, node: var Node, env: var Env): Node

# proc replaceNode(node: Node, original: Node, newNode: Node): Node

# proc registerImport(compiler: var Compiler, label: string)

# proc compileDocstring(compiler: var Compiler, node: Node, env: var Env): seq[string]

# proc monitorCollisions(compiler: var Compiler, label: string)

# proc elementOf(typ: Type): Type

# proc firstElementOf(typ: Type): Type

# proc secondElementOf(typ: Type): Type

# proc findSource(compiler: var Compiler, node: Node): string

# proc typed(node: var Node, typ: Type): Node =
#   node.typ = typ
#   result = node

# proc collapse(node: Node): seq[Node] =
#   case node.kind:
#   of Sequence:
#     result = @[]
#     for child in node.children.mitems:
#       result = result.concat(collapse(child))
#   else:
#     result = @[node]

# proc generalize(typ: Type): Type =
#   assert typ.kind == N.Function

#   result = Type(kind: N.Function, functionArgs: typ.functionArgs.mapIt(atomType("auto")), returnType: atomType("auto"))

# proc genericCompatible(label: string, a: Type, b: Type): (bool, Table[string, Type]) =
#   log fmt"genericCompatible? {label} {$a} @ {$b}"
#   var genericMap = initTable[string, Type]()
#   if a.isNil or b.isNil:
#     return (false, genericMap)
#   # TODO: deep
#   if a.kind != N.Function or b.kind != N.Function:
#     return (false, genericMap)
#   if len(a.functionArgs) == 0 or len(a.functionArgs) != len(b.functionArgs):
#     return (false, genericMap)
#   genericMap["T"] = a.functionArgs[0]
#   for z in 0..<len(a.functionArgs):
#     if not a.functionArgs[z].unify(b.functionArgs[z], genericMap):
#       return (false, genericMap)
#   return (true, genericMap)

# proc genericCompatible(label: string, a: Node, b: Node): (bool, Table[string, Type]) =
#   var genericMap = initTable[string, Type]()
#   if a.kind != PyFunctionDef or b.kind != PyFunctionDef:
#     return (false, genericMap)
#   result = genericCompatible(label, a.typ, b.typ)
#   if result[0]:
#     return (a[2].testEq(b[2]), genericMap)

# proc compileModule*(compiler: var Compiler, file: string, node: Node): Module =
#   var moduleEnv = compiler.envs[file]
#   compiler.currentClass = nil
#   compiler.depth = 0
#   var childNodes: seq[Node] = @[]
#   for child in node.mitems:
#     childNodes.add(compiler.compileNode(child, moduleEnv))
#   var collapsedNodes: seq[Node] = @[]
#   for child in childNodes.mitems:
#     collapsedNodes = collapsedNodes.concat(collapse(child))
#   result = compiler.modules[file]
#   var functions: seq[Node] = @[]
#   for child in collapsedNodes:
#     case child.kind:
#     of PyImport:
#       result.imports.add(child)
#     of PyClassDef:
#       result.types.add(child)
#     of PyFunctionDef:
#       functions.add(child)
#     else:
#       result.init.add(child)

#   # var maybeGeneric = initTable[string, (Node, bool, Table[string, Type])]()
#   # for function in functions:
#   #   if function[0].kind != PyStr:
#   #     continue
#   #   elif not maybeGeneric.hasKey(function[0].s):
#   #     maybeGeneric[function[0].s] = (function, false, initTable[string, Type]())
#   #   else:
#   #     var (equivalent, genericMap) = genericCompatible(function[0].s, maybeGeneric[function[0].s][0], function)
#   #     if equivalent:
#   #       maybeGeneric[function[0].s][0].isGeneric = true
#   #       maybeGeneric[function[0].s] = (function, true, genericMap)
#   #       function.isGeneric = true

#   # for label, f2 in maybeGeneric:
#   #   var (function, isGeneric, genericMap) = f2
#   #   if isGeneric:
#   #     var f = function
#   #     result.functions.add(replaceGeneric(f, genericMap))

#   var maybeGeneric = initTable[string, (seq[Node], bool)]()
#   for function in functions:
#     if function[0].kind != PyStr:
#       continue

#     var label = function[0].s
#     if not maybeGeneric.hasKey(label):
#       maybeGeneric[label] = (@[function], false)
#     else:
#       for other in maybeGeneric[label][0]:
#         if len(function[1].children) == len(other[1].children) and function[2].testEq(other[2]):
#           maybeGeneric[label] = (@[function], true)
#           function.isGeneric = true
#           other.isGeneric = true
#         log fmt"generic? {label} {function.typ} @ {other.typ}: {function.isGeneric}"

#       if not function.isGeneric:
#         maybeGeneric[label][0].add(function)

#   for label, f2 in maybeGeneric:
#     var (functions, isGeneric) = f2
#     if isGeneric:
#       var function = functions[0]
#       function.typ = generalize(function.typ)
#       result.functions.add(function)

#   for function in functions:
#     if not function.isGeneric:
#       result.functions.add(function)

# proc compileImport(compiler: var Compiler, node: var Node, env: var Env): Node =
#   # store imports
#   # by default make them false in maybeModules:
#   # only if the module is used, toggle it
#   assert node.kind == PyImport
#   if node[0].kind == Pyalias and node[0][0].kind == PyStr:
#     compiler.maybeModules[node[0][0].s] = false
#     result = Node(kind: PyImport, children: @[pyLabel(node[0][0].s)])
#   else:
#     warn("import")
#     result = Node(kind: Sequence, children: @[])

# proc compileImportFrom(compiler: var Compiler, node: var Node, env: var Env): Node =
#   # from x import original
#   #
#   # import x
#   #
#   # from x import original as alias
#   #
#   # import x
#   # alias = original

#   if node.kind != PyImportFrom or node[0].kind != PyStr:
#     return Node(kind: Sequence, children: @[])
#   let m = node[0].s
#   var aliases: seq[Node] = @[]
#   for child in node[1]:
#     if child.kind == Pyalias:
#       assert child[0].kind == PyStr
#       let original = child[0].s
#       let alias = if child[1].kind == PyStr: child[1].s else: original
#       let fullName = fmt"{compiler.db.package}.{m}#{original}"
#       if compiler.db.types.hasKey(fullName):
#         env[alias] = compiler.db.types[fullName]
#       if original != alias:
#         aliases.add(assign(label(alias), attribute(label(m), original), Declaration.Var))
#   result = Node(kind: PyImport, children: @[pyLabel(m)], aliases: aliases)
#   compiler.maybeModules[fmt"{compiler.base}/{m}.py"] = true

# proc compileAssign(compiler: var Compiler, node: var Node, env: var Env): Node =
#   let value = compiler.compileNode(node[1], env)
#   if len(node[0].children) > 1:
#     warn("assign")
#   elif node[0][0].kind == PyLabel:
#     var label = node[0][0].label
#     node[1] = value
#     if not env.types.hasKey(label):
#       if compiler.path.endsWith("constants.py"):
#         # weird?
#         # most libraries I test have it
#         # so use it for now, if we need, we can make add a flag
#         node.declaration = Declaration.Const
#         if compiler.constants.hasKey(label) and compiler.constants[label] != value.typ:
#           warn(fmt"const type: {label}")
#         compiler.constants[label] = value.typ
#       else:
#         node.declaration = Declaration.Var
#     env[label] = value.typ
#     compiler.monitorCollisions(label)
#   elif node[0][0].kind == PySubscript:
#     node[0][0] = compiler.compileNode(node[0][0], env)
#     node[1] = value
#   result = node

# proc compilePrint(compiler: var Compiler, name: string, args: seq[Node], env: var Env): Node =
#   # variadic
#   var printArgs = args
#   # for z, arg in args:
#   #   if arg.typ != T.String:
#   #     printArgs[z] = call(label("$"), @[arg], T.String)
#   result = call(label("echo"), printArgs, T.Void)

# proc compileSpecialStrMethod(compiler: var Compiler, name: string, args: seq[Node], env: var Env): Node =
#   result = call(label("$"), args, T.String)

# proc compileLen(compiler: var Compiler, name: string, args: seq[Node], env: var Env): Node =
#   if len(args) != 1:
#     result = call(Node(kind: PyLabel, label: name), args, NIM_ANY)
#   else:
#     result = call(Node(kind: PyLabel, label: name), args, T.Int)

# proc compileReversed(compiler: var Compiler, name: string, args: seq[Node], env: var Env): Node =
#   if len(args) == 1:
#     result = call(Node(kind: PyLabel, label: name), args, args[0].typ)
#     compiler.registerImport("algorithm")
#   else:
#     result = call(Node(kind: PyLabel, label: name), args, NIM_ANY)

# proc compileIsinstance(compiler: var Compiler, name: string, args: seq[Node], env: var Env): Node =
#   result = Node(kind: NimOf, children: args, typ: T.Bool)

# proc compileSpecialIntMethod(compiler: var Compiler, name: string, args: seq[Node], env: var Env): Node =
#   var arg = args[0]
#   result = attribute(arg, name, T.Int)

# proc compileSpecialFloatMethod(compiler: var Compiler, name: string, args: seq[Node], env: var Env): Node =
#   var arg = args[0]
#   result = attribute(arg, name, T.Float)

# proc compileBytes(compiler: var Compiler, name: string, args: seq[Node], env: var Env): Node =
#   var arg: Node
#   if args[0].kind == PyStr:
#     arg = args[0]
#   elif args[0].kind == PyList:
#     var list = args[0]
#     for z, next in list.children:
#       list[z] = attribute(next, "char")
#     arg = call(attribute(list, "join"), @[pyString("")])
#     compiler.registerImport("strutils")
#   result = call(Node(kind: PyLabel, label: "cstring"), @[arg], T.Bytes)

# proc compileEnumerate(compiler: var Compiler, name: string, args: seq[Node], env: var Env): Node =
#   if len(args) != 1:
#     warn("weird enumerate")
#     result = PY_NIL
#   else:
#     result = args[0]

# proc compileAbs(compiler: var Compiler, name: string, args: seq[Node], env: var Env): Node =
#   if len(args) != 2:
#     warn(fmt"{name} expects 2 args")
#     result = PY_NIL
#   else:
#     result = call(Node(kind: PyLabel, label: name), args, args[0].typ)

# var BUILTIN* = {
#   "print": "echo"
# }.toTable()

# var SPECIAL_FUNCTIONS* = {
#   "print": compilePrint,
#   "str": compileSpecialStrMethod,
#   "len": compileLen,
#   "reversed": compileReversed,
#   "isinstance": compileIsinstance,
#   "int": compileSpecialIntMethod,
#   "float": compileSpecialFloatMethod,
#   "bytes": compileBytes,
#   "enumerate": compileEnumerate,
#   "min": compileAbs,
#   "max": compileAbs
# }.toTable()

# proc compileCall*(compiler: var Compiler, node: var Node, env: var Env): Node =
#   let function = compiler.compileNode(node[0], env)
#   let args = compiler.compileNode(node[1], env)
#   if function.typ.isNil and (function.kind != PyLabel or function.label notin SPECIAL_FUNCTIONS):
#     warn("failed to determine type of function:\n" & $node)
#     function.typ = NIM_ANY

#   var imports: seq[string] = @[]
#   if function.kind == PyAttribute:
#     assert function[1].kind == PyStr
#     (result, imports) = maybeApplyMethodIdiom(node, function[0], function[1].s, args.children)
#   elif function.kind == PyLabel:
#     if function.label in SPECIAL_FUNCTIONS:
#       result = SPECIAL_FUNCTIONS[function.label](compiler, function.label, args.children, env)
#     else:
#       (result, imports) = maybeApplyMethodIdiom(node, nil, function.label, args.children)
#   else:
#     result = node
#   for imp in imports:
#     compiler.registerImport(imp)
#   if result.isNil: # no idiom
#     result = node
#     case function.typ.kind:
#     of N.Function:
#       result.typ = function.typ.returnType
#     of N.Any:
#       result.typ = function.typ
#     of N.Record:
#       if function.kind != PyLabel or function.label != function.typ.label:
#         var call = fmt"{function.typ.fullLabel}#__call__"
#         if compiler.db.types.hasKey(call):
#           var typ = compiler.db.types[call]
#           if not typ.isNil and typ.kind == N.Function:
#             function.typ = typ
#             result.typ = typ.returnType
#             return
#         result.typ = NIM_ANY
#       else:
#         result.kind = PyConstr
#         result[2] = result[1]
#       var members: seq[Node] = @[]
#       for member, _ in function.typ.members:
#         members.add(label(member))
#       result[1] = Node(kind: Sequence, children: members)
#       result.typ = function.typ
#     of N.Overloads:
#       result.typ = NIM_ANY
#       for overload in function.typ.overloads:
#         if overload.kind == N.Function and args.children.zip(overload.functionArgs).allIt(it[0].typ == it[1]):
#           result.typ = overload.returnType
#           break
#     else:
#       warn fmt"wtf {function.typ}"
#       result.typ = function.typ
#   if compiler.currentCalls.isValid() and function.kind == PyLabel:
#     compiler.currentCalls.incl(function.label)

# proc monitorCollisions(compiler: var Compiler, label: string) =
#   var translated = translateIdentifier(label)
#   if not compiler.identifierCollisions.hasKey(translated):
#     compiler.identifierCollisions[translated] = (label, false)
#   elif compiler.identifierCollisions[translated][0] != label:
#     compiler.identifierCollisions[translated] = (label, true)

# proc compileLabel(compiler: var Compiler, node: var Node, env: var Env): Node =
#   assert node.kind == PyLabel
#   if node.label in SPECIAL_FUNCTIONS:
#     result = node
#   elif node.label == "True" or node.label == "False":
#     result = Node(kind: PyLabel, label: node.label.toLowerAscii(), typ: T.Bool)
#   elif node.label == "None":
#     result = PY_NIL
#   else:
#     var typ = env.get(node.label)
#     if typ.isNil:
#       typ = env.get(fmt"{compiler.currentModule}.{node.label}")
#     result = typed(node, typ)

#   compiler.monitorCollisions(node.label)

# proc compileStr(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = typed(node, T.String)

# proc compileInt(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = typed(node, T.Int)

# proc compileHugeInt(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = typed(node, T.HugeInt)

# proc compileFloat(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = typed(node, T.Float)

# proc compileConstant(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = typed(node, T.Bool)

# proc compileExpr(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = compiler.compileNode(node[0], env)

# proc compileBinOp(compiler: var Compiler, node: var Node, env: var Env): Node =
#   var left = compiler.compileNode(node[0], env)
#   var right = compiler.compileNode(node[2], env)
#   let op = node[1]
#   if left.typ == T.Float and right.typ == T.Int:
#     right = attribute(right, "float", T.Float)
#   elif left.typ == T.Int and right.typ == T.Float:
#     left = attribute(left, "float", T.Float)
#   elif right.typ.isNil and left.typ.isNil:
#     right.typ = NIM_ANY
#     left.typ = NIM_ANY
#   elif left.typ.isNil or left.typ.kind == N.Any:
#     left.typ = right.typ
#   elif right.typ.isNil or right.typ.kind == N.Any:
#     right.typ = left.typ
#   node[0] = left
#   node[2] = right
#   result = node
#   var imports: seq[string] = @[]

#   if left.typ == T.Int or left.typ == T.Float:
#     result.typ = left.typ
#     # TODO:
#     (result, imports) = applyOperatorIdiom(result)
#   else:
#     (result, imports) = applyOperatorIdiom(result)
#   if result.isNil:
#     var typ = left.typ
#     result = binop(left, op, right, typ)
#   for imp in imports:
#     compiler.registerImport(imp)

# proc pureConstr(compiler: var Compiler, node: var Node): bool =
#   let args = node[1][0].mapIt(it[0].s)
#   for child in node[2]:
#     if child.kind != PyAssign or len(child[0].children) != 1 or child[0][0].kind != PyAttribute or
#        child[0][0][0].kind != PyLabel or child[0][0][0].label != "self" or child[0][0][1].kind != PyStr or
#        child[0][0][1].s notin args or child[1].kind != PyLabel or child[1].label != child[0][0][1].s:
#        return false
#   return true

# proc translateInit(compiler: var Compiler, node: var Node, env: var Env, child: bool = false, assignments: seq[Node] = @[]): Node =
#   result = node
#   if not child:
#     result[0].s = compiler.currentClass.init
#     result[1][0].children = result[1][0].children[1..^1]
#     if result.typ.isNil or result.typ.kind != N.Function:
#       warn("missing init type")
#       result.typ = Type(kind: N.Function, functionArgs: repeat(NIM_ANY, len(result[1][0].children)))
#     result.typ.functionArgs = result.typ.functionArgs[1..^1]
#     result.typ.returnType = compiler.currentClass
#     result[2].children = assignments.concat(result[2].children)
#     if compiler.currentClass.isRef:
#       result[2].children = @[call(label("new"), @[label("result")])].concat(result[2].children)


#   case node.kind:
#   of PyLabel:
#     if node.label == "self":
#       result.label = "result"
#   else:
#     var z = 0
#     for next in result.mitems:
#       result[z] = compiler.translateInit(next, env, true)
#       z += 1

# proc replaceReturnYield(node: Node): Node =
#   if node.kind == PyReturn:
#     return Node(kind: PyYield, children: node.children)
#   else:
#     result = node
#     var z = 0
#     for child in node:
#       result[z] = replaceReturnYield(child)
#       z += 1

# proc loadBranches(node: Node, nimElif: bool = false): seq[Node] =
#   case node.kind:
#   of Sequence:
#     if len(node.children) != 1:
#       result = @[]
#     else:
#       result = loadBranches(node[0])
#   of PyIf:
#     var ifBranch = if not nimElif: Node(kind: NimIf, children: @[node[0], node[1]]) else: Node(kind: NimElif, children: @[node[0], node[1]])
#     case node[2].kind:
#     of PyNone:
#       result = @[ifBranch]
#     of Sequence:
#       if len(node[2].children) == 1 and node[2][0].kind == PyIf:
#         result = @[ifBranch].concat(loadBranches(node[2][0], true))
#       else:
#         var elseBranch = Node(kind: NimElse, children: @[node[2][0]])
#         result = @[ifBranch, elseBranch]
#     of PyIf:
#       result = @[ifBranch].concat(loadBranches(node[2], true))
#     else:
#       var elseBranch = Node(kind: NimElse, children: @[node[2]])
#       result = @[ifBranch, elseBranch]
#   else:
#     result = @[]

# proc isDynamicTest(test: Node, args: HashSet[string]): bool =
#   result = test.kind == PyCall and test[0].kind == PyLabel and test[0].label in @["hasattr", "isinstance"] and len(test[1].children) > 1 and test[1][0].kind == PyLabel and test[1][0].label in args

# proc generateDynamic(compiler: var Compiler, test: Node): Type =
#   if test.kind == PyCall and test[0].kind == PyLabel:
#     var check = test[0].label
#     if check == "hasattr" and test[1][1].kind == PyStr:
#       result = Type(kind: N.Macro, label: "HasField", macroArgs: @[atomType(test[1][1].s)])
#       compiler.registerImport("py2nim_helpers")
#     elif check == "isinstance" and test[1][1].kind == PyLabel:
#       var label = test[1][1].label
#       if label == "list":
#         result = T.List
#       elif label == "dict":
#         result = T.Dict
#       else:
#         result = toType(PyType(kind: PyTypeAtom, label: label))
#     else:
#       result = NIM_ANY
#   else:
#     result = NIM_ANY

# proc generateBranch(compiler: var Compiler, branch: Node, node: Node, typ: Type): Node =
#   if branch.kind in {NimIf, NimElif}:
#     var label = branch[0][1][0].label
#     result = Node(kind: PyFunctionDef, children: @[deepCopy(node[0]), deepCopy(node[1]), deepCopy(branch[1])])
#     for z, arg in node[1][0].nitems:
#       if arg[0].s == label:
#         result.typ = deepCopy(typ)
#         if result.typ.kind == N.Overloads:
#           result.typ = result.typ.overloads[0]
#         result.typ.functionArgs[z] = compiler.generateDynamic(branch[0])
#         break
#   elif branch.kind == NimElse:
#     result = Node(kind: PyFunctionDef, typ: typ, children: @[deepCopy(node[0]), deepCopy(node[1]), deepCopy(branch[0])])
#   else:
#     warn $branch.kind
#     result = PY_NIL

# proc dynamicBranches(compiler: var Compiler, node: Node, typ: Type): (bool, seq[Node]) =
#   if node.kind != PyFunctionDef or typ.isNil or typ.kind notin {N.Function, N.Overloads}:
#     return (false, @[])

#   var branches: seq[Node] = @[]

#   var args = initSet[string]()
#   for arg in node[1][0]:
#     args.incl(arg[0].s)

#   var children = loadBranches(node[2])
#   for child in children:
#     if child.kind in {NimIf, NimElif} and
#        not child[0].isDynamicTest(args):
#       return (false, @[])

#   for child in children:
#     branches.add(compiler.generateBranch(child, node, typ))

#   result = (len(branches) > 0, branches)





# proc compileFunctionDef(compiler: var Compiler, node: var Node, env: var Env, assignments: seq[Node] = @[], fTyp: Type = nil): Node =
#   assert node.kind == PyFunctionDef

#   var f = node[0]
#   assert f.kind == PyStr # TODO: label
#   var label = f.s

#   compiler.monitorCollisions(label)

#   # analyze typr
#   # ignore weird functions
#   # if we find an overloaded function, call this function for each overload

#   node.calls = initSet[string]()
#   let typ = if fTyp.isNil: env.get(label) else: fTyp
#   if typ.isNil or typ.kind notin {N.Overloads, N.Function}:
#     return Node(kind: Sequence, children: @[])

#   var (isDynamic, branches) = compiler.dynamicBranches(node, typ)
#   if isDynamic:
#     result = Node(kind: Sequence, children: @[])
#     for branch in branches.mitems:
#       result.children.add(compiler.compileFunctionDef(branch, env, fTyp=branch.typ))
#     return
#   elif typ.kind == N.Overloads:
#     result = Node(kind: Sequence, children: @[])
#     let originalNode = node
#     for overload in typ.overloads:
#       log fmt"compile {overload}"
#       var tempNode = deepCopy(originalNode)
#       result.children.add(compiler.compileFunctionDef(tempNode, env, fTyp=overload))
#     return

#   # analyze magic methods

#   var isInit = false
#   if label == "__init__":
#     if len(assignments) == 0 and compiler.pureConstr(node):
#       compiler.currentClass.init = ""
#       return Node(kind: Sequence, children: @[])
#       # TODO mark it so we can use PyConstr
#       # and rename to newType, change return type and remove self otherwise
#       # idiomatic function
#     else:
#       if compiler.currentClass.isRef:
#         compiler.currentClass.init = fmt"new{compiler.currentClass.label}"
#       else:
#         compiler.currentClass.init = fmt"make{compiler.currentClass.label}"
#       isInit = true
#   elif label == "__len__":
#     label = "len"
#     node[0].s = label
#   elif label == "__getitem__":
#     label = "[]"
#     node[0] = Node(kind: NimAccQuoted, children: @[Node(kind: PyLabel, label: label)])
#   elif label == "__setitem__":
#     label = "[]="
#     node[0] = Node(kind: NimAccQuoted, children: @[Node(kind: PyLabel, label: label)])
#   elif label == "__contains__":
#     label = "contains"
#     node[0].s = label
#   elif label == "__delitem__":
#     label = "del"
#     node[0].s = label
#   elif label == "__iter__":
#     if len(node[2].children) != 1 or not node[2][0].testEq(Node(kind: PyReturn, children: @[Node(kind: PyLabel, label: "self")])):
#       warn("def __iter__(self): return self only supported")
#     result = Node(kind: Sequence, children: @[])
#     return
#   elif label == "__next__":
#     node[2] = replaceReturnYield(node[2])
#     node[2] = replaceNode(node[2], Node(kind: PyRaise, children: @[call(Node(kind: PyLabel, label: "StopIteration"), @[], T.Void), PY_NIL]), Node(kind: PyBreak, children: @[]))
#     node[2] = Node(
#       kind: Sequence,
#       children: @[
#         Node(
#           kind: PyWhile,
#           children: @[pyBool(true), node[2]])])
#     label = "items"
#     node[0].s = label
#     typ.functionArgs[0].isVar = true
#     node.isIterator = true
#   elif label == "__enter__":
#     label = "enter"
#     node[0].s = label
#     typ.functionArgs[0].isVar = true
#   elif label == "__exit__":
#     label = "exit"
#     node[0].s = label
#     typ.functionArgs[0].isVar = true
#     if len(typ.functionArgs) == 4:
#       typ.functionArgs[1] = Type(kind: N.Atom, label: "Exception", isRef: true)
#       typ.functionArgs[2] = Type(kind: N.Atom, label: "Exception", isRef: true)
#       typ.functionArgs[3] = T.String
#   elif label == "__str__":
#     label = "$"
#     node[0] = Node(kind: NimAccQuoted, children: @[Node(kind: PyLabel, label: label)])
#   elif label == "__call__":
#     # TODO: use {} in future, () is deprecated
#     label = "()"
#     node[0] = Node(kind: NimAccQuoted, children: @[Node(kind: PyLabel, label: label)])

#   # analyze args

#   var args = initTable[string, Type]()
#   var z = 0
#   for v in node[1][0]:
#     assert v.kind == Pyarg and v[0].kind == PyStr
#     args[v[0].s] = typ.functionArgs[z]
#     z += 1
#     compiler.monitorCollisions(v[0].s)

#   var functionEnv = childEnv(env, label, args, typ.returnType)
#   compiler.currentFunction = typ.fullLabel
#   compiler.currentCalls = initSet[string]()

#   var sequence = node[2]
#   if sequence.kind != Sequence:
#     sequence = Node(kind: Sequence, children: @[sequence])

#   for z, child in sequence.nitems:
#     sequence.children[z] = compiler.compileNode(child, functionEnv)

#   compiler.currentFunction = ""
#   node.calls = compiler.currentCalls
#   typ.returnType = functionEnv.returnType
#   if functionEnv.hasYield:
#     node.isIterator = true
#   if isInit:
#     result = compiler.translateInit(node, env, assignments=assignments)
#   else:
#     result = typed(node, typ)
#   if not compiler.currentClass.isNil and (not compiler.currentClass.base.isNil or compiler.currentClass.inherited):
#     result.isMethod = true
#   if len(node.children) > 5:
#     result.doc = compiler.compileDocstring(node[5], env)
#   elif len(result[2].children) > 0 and result[2][0].kind == PyStr:
#     result.doc = compiler.compileDocstring(result[2][0], env)
#     result[2].children = result[2].children[1..^1]
#   else:
#     result.doc = @[]

# proc compileAttribute*(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = node
#   var base = compiler.compileNode(node[0], env)
#   var typ = base.typ
#   if typ.isNil or typ.kind == N.Any:
#     result.typ = NIM_ANY # TODO: experiment with just generating the same code for some nodes
#     #fail(fmt"no type for node {node}")
#     return
#   var fullName = typ.label
#   var oldTyp = typ
#   if typ.kind == N.Atom:
#     fullName = fmt"{compiler.currentModule}.{typ.label}"
#     typ = env.get(fullName)
#     if typ.isNil and compiler.db.types.hasKey(fullName):
#       typ = compiler.db.types[fullName]
#       env[fullName] = typ
#     elif typ.isNil:
#       typ = oldTyp

#   if typ.kind == N.Record:
#     assert node[1].kind == PyStr
#     if node[1].s notin typ.members:
#       var methodName = fmt"{compiler.currentModule}.{typ.label}#{node[1].s}"
#       if methodName notin compiler.db.types:
#         warn(fmt"no type for {node[1].s} in {fullName}")
#         methodName = fmt"{compiler.currentModule}#{node[1].s}"
#         if methodName notin compiler.db.types:
#           warn(fmt"no type for {node[1].s} in {fullName}")
#           result.typ = NIM_ANY
#         else:
#           result.children[0] = base
#           result.typ = compiler.db.types[methodName]
#       else:
#         result.children[0] = base
#         result.typ = compiler.db.types[methodName]
#     else:
#       result.children[0] = base
#       result.typ = typ.members[node[1].s]
#   else:
#     result.typ = NIM_ANY
#   if not result.typ.isNil and result.typ.kind == N.Function and len(result.typ.functionArgs) == 1:
#     if result.typ.functionArgs[0].kind == N.Atom:
#       result.typ = result.typ.returnType
#     elif result.typ.functionArgs[0].kind == N.Record and result.typ.functionArgs[0].members.hasKey(node[1].s):
#       result.typ = result.typ.returnType

# proc compileSequence*(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = node
#   var z = 0
#   for child in result.children.mitems:
#     result.children[z] = compiler.compileNode(child, env)
#     z += 1

# proc compileList*(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = node
#   var z = 0
#   for child in result.children.mitems:
#     result[z] = compiler.compileNode(child, env)
#     z += 1
#   if len(result.children) > 0:
#     result.typ = seqType(result[0].typ)
#   else:
#     result.typ = seqType(NIM_ANY)


# proc compileReturn(compiler: var Compiler, node: var Node, env: var Env): Node =
#   node[0] = compiler.compileNode(node[0], env)
#   if node[0].typ != env.returnType:
#     # TODO
#     if env.returnType == T.Void:
#       env.returnType = node[0].typ
#     else:
#       warn(fmt"{compiler.currentFunction} expected {$env.returnType} got {$node[0].typ} return")
#   result = node

# proc compileIf(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = node
#   var (line, column) = (node[0].line, node[0].column)
#   if node[0].kind == PyCompare and node[0][0].kind == PyLabel and
#      node[0][0].label == "__name__" and
#      node[0][1][0].kind == PyEq and node[0][2][0].kind == PyStr and node[0][2][0].s == "__main__":
#     result.kind = NimWhen
#     result[0] = Node(kind: PyLabel, label: "isMainModule", typ: T.Bool)
#   else:
#     result[0] = compiler.compileNode(node[0], env)
#     if result[0].typ != T.Bool:
#       var info = findSource(compiler.path, line, column, "test")
#       warn(fmt"expected bool got {$result[0].typ} if: {info}")
#   result[1] = compiler.compileNode(node[1], env)

# proc compileCompare(compiler: var Compiler, node: var Node, env: var Env): Node =
#   # TODO: compound compare
#   var left = compiler.compileNode(node[0], env)
#   var op = node[1][0]
#   var right = compiler.compileNode(node[2][0], env)
#   if not (
#       left.typ == T.Int and right.typ == T.Int or
#       left.typ == T.Float and right.typ == T.Float or
#       op.kind == PyEq or op.kind == PyNotEq):
#     warn(fmt"{$op} {$left.typ} {$right.typ}")
#   result = node
#   result.typ = T.Bool


# proc createInit(compiler: var Compiler, assignments: seq[Node]): Node =
#   if compiler.currentClass.isRef:
#     compiler.currentClass.init = fmt"new{compiler.currentClass.label}"
#   else:
#     compiler.currentClass.init = fmt"make{compiler.currentClass.label}"
#   result = Node(
#     kind: PyFunctionDef,
#     children: @[
#       Node(kind: PyStr, s: compiler.currentClass.init),
#       Node(kind: Pyarguments, children: @[Node(kind: Sequence, children: @[]), PY_NIL, Node(kind: Sequence, children: @[])]),
#       Node(kind: Sequence, children: assignments),
#       PY_NIL])
#   result.typ = Type(kind: N.Function, label: compiler.currentClass.init, functionArgs: @[], returnType: compiler.currentClass)

# proc compileDocstring(compiler: var Compiler, node: Node, env: var Env): seq[string] =
#   if node.kind != PyStr:
#     result = @[]
#   else:
#     result = node.s.split("\\n").mapIt(it.strip(leading=false))
#     if len(result[0]) == 0:
#       result = result[1..^1]
#     if len(result) > 0 and len(result[^1]) == 0:
#       result = result[0..^2]
#     # var unindent = 200
#     # for line in result:
#     #   var offset = 0
#     #   for c in line:
#     #     if c == ' ':
#     #       offset += 1
#     #     else:
#     #       break
#     #   if offset < unindent and offset > 0:
#     #     unindent = offset
#     # result = result.mapIt(if len(it) == 0: it else: it[unindent..^1])


# proc compileClassDef(compiler: var Compiler, node: var Node, env: var Env): Node =
#   assert node[0].kind == PyStr
#   let label = node[0].s
#   var typ = env[label]
#   compiler.currentClass = typ
#   compiler.currentClass.init = ""

#   compiler.monitorCollisions(label)

#   var children = node[3].children
#   if len(node.children) > 5:
#     node.docstring = compiler.compileDocstring(node[5], env)
#   elif len(children) > 0 and children[0].kind == PyExpr and children[0][0].kind == PyStr:
#     node.docstring = compiler.compileDocstring(children[0][0], env)
#     children = children[1..^1]
#   else:
#     node.docstring = @[]
#   node[0] = Node(kind: PyLabel, label: label)

#   if node[1].kind == Sequence and len(node[1].children) > 0:
#     if len(node[1].children) == 1 and node[1][0].kind == PyLabel:
#       typ.base = env.get(node[1][0].label)
#       if typ.base.isNil:
#         typ.base = Type(kind: N.Atom, label: node[1][0].label)

#   result = node

#   if len(children) > 0:
#     result = Node(kind: Sequence, children: @[result])

#     var classEnv = childEnv(env, label, initTable[string, Type](), nil)

#     var z = 0
#     var assignments: seq[Node] = @[]
#     for child in children.mitems:
#       if child.kind == PyFunctionDef:
#         children[z] = compiler.mergeFunctionTypeInfo(child, classEnv)
#       elif child.kind == PyAssign and len(child[0].children) == 1 and child[0][0].kind == PyLabel:
#         var value = compiler.compileNode(child[1], classEnv)
#         assignments.add(assign(attribute(Node(kind: PyLabel, label: "result"), child[0][0].label), value))
#         if not typ.members.hasKey(child[0][0].label):
#           typ.members[child[0][0].label] = value.typ
#       z += 1

#     z = 0
#     var hasInit = false
#     for child in children.mitems:
#       if child.kind == PyFunctionDef:
#         if child[0].kind == PyLabel and child[0].label == "__init__":
#           hasInit = true
#         result.children.add(compiler.compileFunctionDef(child, classEnv, assignments))
#       z += 1
#     if not hasInit and len(assignments) > 0:
#       result.children.add(compiler.createInit(assignments))
#   compiler.currentClass = nil

# proc replaceFile(compiler: var Compiler, node: var Node, handler: string, filename: Node): Node =
#   result = nil
#   if node.kind == PyCall and node[0].kind == PyAttribute and node[0][0].kind == PyLabel and
#      node[0][0].label == handler:
#     result = nil
#     if node[0][1].s == "read" and len(node[1].children) == 0:
#       result = call(label("readFile"), @[filename], T.String)
#       result.ready = true
#     elif node[0][1].s == "write" and len(node[1].children) == 1:
#       let arg = node[1][0]
#       result = call(label("writeFile"), @[filename, arg], T.Void)
#       result.ready = true
#   if result.isNil:
#     result = node
#     var z = 0
#     for child in node.mitems:
#       result[z] = compiler.replaceFile(child, handler, filename)
#       z += 1

# proc compileWith(compiler: var Compiler, node: var Node, env: var Env): Node =
#   # with open(filename, mode) as f:
#   #    code ..
#   #    f.read() / f.write(source)
#   #   .. code
#   #
#   # is translated to
#   #
#   # code ..
#   # readFile() / writeFile(source)
#   # .. code
#   # TODO: append etc
#   # TODO: other common context

#   assert node.kind == PyWith

#   assert node[0][0].kind == Pywithitem

#   var header = node[0][0][0]
#   var handler = node[0][0][1]
#   var code = node[1]
#   if header.kind == PyCall and header[0].kind == PyLabel and header[0].label == "open" and
#      handler.kind == PyLabel:
#     let filename = header[1][0]
#     result = compiler.replaceFile(code, handler.label, filename)
#     result = compiler.compileNode(result, env)
#   else:
#     result = Node(
#       kind: PyWith,
#       children: @[
#         Node(
#           kind: Pywithitem,
#           children: @[
#             compiler.compileNode(header, env),
#             compiler.compileNode(handler, env)]),
#         compiler.compileNode(code, env)])
#     compiler.registerImport("py2nim_helpers")

# proc compileFor*(compiler: var Compiler, node: var Node, env: var Env): Node =
#   # for element in a:
#   #   code
#   #
#   # doesn't change
#   #
#   # for z, element in enumerate(a):
#   #   code
#   #
#   # becomes
#   #
#   # for z, element in a:
#   #   code
#   #
#   # for k, v in a.items(): # a is dict
#   #    code
#   #
#   # becomes
#   #
#   # for k, v in a:
#   #   code
#   #
#   # for z in range([start], finish, [step]):
#   #   code
#   #
#   # becomes
#   #
#   # for z in [start]..<finish: / for z in countup(start, finish, step):
#   #   code
#   # TODO
#   # else

#   let element = node[0]
#   let sequence = node[1]
#   let code = node[2]

#   if sequence.kind == PyCall and sequence[0].kind == PyAttribute and sequence[0][1].kind == PyStr and sequence[0][1].s == "items":
#     let candidateDict = compiler.compileNode(sequence[0][0], env)
#     node[1] = candidateDict
#     if element.kind == PyLabel:
#       element.typ = firstElementOf(candidateDict.typ)
#       env[element.label] = element.typ
#     elif element.kind == PyTuple and element[0].kind == PyLabel and element[1].kind == PyLabel:
#       element[0].typ = firstElementOf(candidateDict.typ)
#       element[1].typ = secondElementOf(candidateDict.typ)
#       env[element[0].label] = element[0].typ
#       env[element[1].label] = element[1].typ
#   elif sequence.kind == PyCall and sequence[0].kind == PyLabel and sequence[0].label == "enumerate":
#     let candidateList = compiler.compileNode(sequence[1][0], env)
#     node[1] = candidateList
#     if element.kind == PyTuple and element[0].kind == PyLabel and element[1].kind == PyLabel:
#       element[0].typ = T.Int
#       element[1].typ = elementOf(candidateList.typ)
#       env[element[0].label] = element[0].typ
#       env[element[1].label] = element[1].typ
#   elif sequence.kind == PyCall and sequence[0].kind == PyLabel and sequence[0].label == "range":
#     var start: Node
#     var finish: Node
#     if len(sequence[1].children) == 1:
#       start = pyInt(0)
#       finish = sequence[1][0]
#       node[1] = Node(kind: NimRangeLess, children: @[start, finish])
#     elif len(sequence[1].children) == 2:
#       start = sequence[1][0]
#       finish = sequence[1][1]
#       node[1] = Node(kind: NimRangeLess, children: @[start, finish])
#     elif len(sequence[1].children) == 3:
#       node[1][0].label = "countup"
#     if element.kind == PyLabel:
#       element.typ = T.Int
#       env[element.label] = element.typ
#   elif sequence.kind == PyCall and sequence[0].kind == PyLabel and sequence[0].label == "zip" and len(sequence[1].children) == 2:
#     let first = compiler.compileNode(sequence[1][0], env)
#     let second = compiler.compileNode(sequence[1][1], env)
#     if element.kind == PyTuple and len(element.children) == 2 and element[0].kind == PyLabel and element[1].kind == PyLabel:
#       element[0].typ = elementOf(first.typ)
#       element[1].typ = elementOf(second.typ)
#       env[element[0].label] = element[0].typ
#       env[element[1].label] = element[1].typ
#     node[1] = call(Node(kind: PyLabel, label: "zip"), @[first, second], T.Void)
#     compiler.registerImport("sequtils")
#   else:
#     node[1] = compiler.compileNode(node[1], env)
#   node[2] = compiler.compileNode(node[2], env)

#   result = node

# proc registerImport(compiler: var Compiler, label: string) =
#   var module = compiler.modules[compiler.path]
#   for imp in module.imports.mitems:
#     if imp.children[0].label == label:
#       return

#   compiler.modules[compiler.path].imports.add(Node(kind: PyImport, children: @[Node(kind: PyLabel, label: label)], aliases: @[]))
#   compiler.monitorCollisions(label)

# proc compileDict(compiler: var Compiler, node: var Node, env: var Env): Node =
#   if len(node[0].children) > 0:
#     node[0] = compiler.compileNode(node[0], env)
#     node[1] = compiler.compileNode(node[1], env)
#     node.typ = tableType(node[0][0].typ, node[1][0].typ)
#   else:
#     node.typ = tableType(NIM_ANY, NIM_ANY)
#   result = node
#   compiler.registerImport("tables")

# proc toBool(test: Node): Node =
#   if test.typ == T.Bool:
#     result = test
#   elif test.typ == T.Int:
#     result = compare(notEq(), test, 0, T.Bool)
#   elif test.typ == T.Float:
#     result = compare(notEq(), test, 0.0, T.Bool)
#   elif test.typ == T.String:
#     result = compare(notEq(), test, pyString(""), T.Bool)
#   else:
#     result = Node(kind: PyUnaryOp, children: @[operator("not "), call(attribute(test, "isNil"), @[], T.Bool)], typ: T.Bool)

# proc compileWhile(compiler: var Compiler, node: var Node, env: var Env): Node =
#   var test = compiler.compileNode(node[0], env)
#   if test.typ != T.Bool:
#     test = toBool(test)
#   node[0] = test
#   node[1] = compiler.compileNode(node[1], env)
#   result = node

# proc findSource(compiler: var Compiler, node: Node): string =
#   result = findSource(compiler.path, node.line, node.column, $node)

# proc commentedOut(s: string): Node =
#   result = Node(kind: NimCommentedOut, children: @[Node(kind: PyStr, s: s, typ: T.String)], typ: NIM_ANY)

# proc commentedOut(compiler: var Compiler, node: Node): Node =
#   var exp = compiler.findSource(node)
#   result = commentedOut(exp)

# proc replaceNode(node: Node, original: Node, newNode: Node): Node =
#   if node.testEq(original):
#     result = newNode
#   else:
#     result = node
#     var z = 0
#     for child in node.mitems:
#       result[z] = replaceNode(child, original, newNode)
#       z += 1

# proc elementOf(typ: Type): Type =
#   if typ.isList():
#     result = typ.args[0]
#   elif typ.isDict():
#     result = typ.args[0]
#   elif typ == T.String:
#     result = T.Char
#   elif typ == T.Bytes:
#     result = T.Int
#   else:
#     result = NIM_ANY

# proc firstElementOf(typ: Type): Type =
#   if typ.isList():
#     result = T.Int
#   elif typ.isDict():
#     result = typ.args[0]
#   else:
#     result = T.Int

# proc secondElementOf(typ: Type): Type =
#   if typ.isList():
#     result = typ.args[0]
#   elif typ.isDict():
#     result = typ.args[1]
#   elif typ == T.String:
#     result = T.Char
#   elif typ == T.Bytes:
#     result = T.Int
#   else:
#     result = NIM_ANY

# proc compileListComp(compiler: var Compiler, node: var Node, env: var Env): Node =
#   # [code for element in a]
#   #
#   # becomes
#   #
#   # a.mapIt(code) # element becomes it
#   #
#   # [code for element in a if test]
#   #
#   # becomes
#   #
#   # a.filterIt(test).mapIt(code) # element becomes it


#   assert node[1][0].kind == Pycomprehension and node[1][0][3].kind == PyInt and node[1][0][3].i == 0

#   let sequence = compiler.compileNode(node[1][0][1], env)
#   if not sequence.typ.isList() and
#      not sequence.typ.isDict() and
#      sequence.typ != T.String and
#      sequence.typ != T.Bytes:
#     warn(fmt"list comprehension which is not on iterable might fail: {$sequence.typ}")
#   var element = node[1][0][0]
#   var code = node[0]
#   if element.kind != PyLabel:
#     warn("only list comprehension with `element` in supported")
#     return compiler.commentedOut(node)
#   let types = {"it": elementOf(sequence.typ)}.toTable()
#   var codeEnv = childEnv(env, "<code>", types, nil)
#   var mapIt = replaceNode(code, element, Node(kind: PyLabel, label: "it"))
#   let mapCode = compiler.compileNode(mapIt, codeEnv)
#   if len(node[1][0][2].children) > 0:
#     var test = node[1][0][2][0]
#     var filterIt = replaceNode(test, element, Node(kind: PyLabel, label: "it"))
#     let filterCode = compiler.compileNode(filterIt, codeEnv)
#     result = call(attribute(call(attribute(sequence, "filterIt"), @[filterCode]), "mapIt"), @[mapCode], seqType(mapCode.typ))
#   else:
#     result = call(attribute(sequence, "mapIt"), @[mapCode], seqType(mapCode.typ))
#   compiler.registerImport("sequtils")

# proc compileGeneratorExp(compiler: var Compiler, node: var Node, env: var Env): Node =
#   # for now we'll just close our eyes and send it to our brother to translate it
#   result = compiler.compileListComp(node, env)


# proc compileDictComp(compiler: var Compiler, node: var Node, env: var Env): Node =
#   # {k: v for k, v in dict.items()}
#   #
#   # becomes
#   #
#   # dict.mapTable(k:v) # k and v are magical
#   #
#   # {k: v for element in list}
#   #
#   # becomes
#   #
#   # list.mapTable(k:v) # element becomes it

#   var sequence: Node
#   if node[2][0][1].kind == PyCall and node[2][0][1][0].kind == PyAttribute and
#      node[2][0][1][0][1].kind == PyStr and node[2][0][1][0][1].s == "items":
#     sequence = compiler.compileNode(node[2][0][1][0][0], env)
#   else:
#     sequence = compiler.compileNode(node[2][0][1], env)
#   if not sequence.typ.isDict() and
#      not sequence.typ.isList() and
#      sequence.typ != T.String and
#      sequence.typ != T.Bytes:
#     warn(fmt"dict comp without iterable might fail: {$sequence.typ}")
#     return compiler.commentedOut(node)

#   var element = node[2][0][0]
#   var key = node[0]
#   var value = node[1]
#   var types: Table[string, Type]
#   var keyReplaced: Node
#   var valueReplaced: Node
#   var test: Node
#   if len(node[2][0][2].children) > 0:
#     test = node[2][0][2][0]

#   if element.kind == PyLabel:
#     types = {"it": elementOf(sequence.typ)}.toTable()
#     keyReplaced = replaceNode(key, element, Node(kind: PyLabel, label: "it"))
#     valueReplaced = replaceNode(value, element, Node(kind: PyLabel, label: "it"))
#     if not test.isNil:
#       test = replaceNode(test, element, Node(kind: PyLabel, label: "it"))
#   elif element.kind == PyTuple and element[0].kind == PyLabel and element[1].kind == PyLabel:
#     types = {"k": firstElementOf(sequence.typ), "v": secondElementOf(sequence.typ)}.toTable()
#     keyReplaced = replaceNode(key, element[0], Node(kind: PyLabel, label: "k"))
#     keyReplaced = replaceNode(keyReplaced, element[1], Node(kind: PyLabel, label: "v"))
#     valueReplaced = replaceNode(value, element[0], Node(kind: PyLabel, label: "k"))
#     valueReplaced = replaceNode(value, element[1], Node(kind: PyLabel, label: "v"))
#     if not test.isNil:
#       test = replaceNode(test, element[0], Node(kind: PyLabel, label: "k"))
#       test = replaceNode(test, element[1], Node(kind: PyLabel, label: "v"))
#   var base: Node
#   var codeEnv = childEnv(env, "<code>", types, nil)
#   if test.isNil:
#     base = sequence
#   else:
#     let testCode = compiler.compileNode(test, codeEnv)
#     let fn = if element.kind == PyLabel: "filterIt" else: "filterTable"
#     base = call(attribute(sequence, fn), @[testCode])
#   let keyCode = compiler.compileNode(keyReplaced, codeEnv)
#   let valueCode = compiler.compileNode(valueReplaced, codeEnv)
#   let pairCode = @[Node(kind: NimTuple, children: @[keyCode, valueCode])]
#   result = call(attribute(base, "mapTable"), pairCode, tableType(keyCode.typ, valueCode.typ))
#   if element.kind == PyLabel:
#     compiler.registerImport("sequtils")
#   compiler.registerImport("tables")
#   compiler.registerImport("py2nim_helpers")


# let EXCEPTIONS = {
#   "IndexError": "IndexError",
#   "ValueError": "ValueError",
# }.toTable()

# proc compileException(compiler: var Compiler, label: string, env: var Env): Node =
#   if EXCEPTIONS.hasKey(label):
#     result = Node(kind: PyLabel, label: EXCEPTIONS[label])
#   else:
#     result = Node(kind: PyLabel, label: label)

# proc compileTry(compiler: var Compiler, node: var Node, env: var Env): Node =
#   # try:
#   #   code
#   # except E as e:
#   #   print(e)
#   #   handler
#   #
#   # becomes
#   #
#   # try:
#   #   code
#   # except NimE:
#   #   echo getCurrentExceptionMsg()
#   #   handler
#   #
#   # TODO finally
#   result = Node(kind: PyTry, children: @[])
#   var code = compiler.compileNode(node[0], env)
#   result.children.add(code)
#   result.children.add(Node(kind: Sequence, children: @[]))
#   for handler in node[1]:
#     if handler.kind == PyExceptHandler:
#       var exception = handler[0]
#       if exception.kind == PyLabel:
#         exception = compiler.compileException(exception.label, env)
#       else:
#         exception = compiler.compileNode(exception, env)
#       var e = handler[1]
#       var handlerCode = compiler.compileNode(handler[2], env)
#       if e.kind == PyStr:
#         handlerCode = replaceNode(handlerCode, Node(kind: PyLabel, label: e.s), call(Node(kind: PyLabel, label: "getCurrentExceptionMsg"), @[], T.String))
#       result[1].children.add(Node(kind: PyExceptHandler, children: @[exception, handlerCode]))

# proc compileSubscript(compiler: var Compiler, node: var Node, env: var Env): Node =
#   node[0] = compiler.compileNode(node[0], env)
#   node[1] = compiler.compileNode(node[1], env)
#   var typ: Type
#   if node[1].kind == PySlice:
#     typ = node[0].typ
#   elif node[0].typ.isList() and node[1].typ == T.Int:
#     typ = node[0].typ.args[0]
#   elif node[0].typ.isDict() and node[1].typ == node[0].typ.args[0]:
#     typ = node[0].typ.args[1]
#   elif node[0].typ == T.String:
#     typ = T.Char
#   elif node[0].typ == T.Bytes:
#     typ = T.Int
#   elif len(node[0].typ.fullLabel) > 0:
#     var getitem = fmt"{node[0].typ.fullLabel}#__getitem__"
#     if compiler.db.types.hasKey(getitem):
#       typ = compiler.db.types[getitem]
#       if typ.kind == N.Function:
#         typ = typ.returnType
#       else:
#         typ = NIM_ANY
#     else:
#       typ = NIM_ANY
#   else:
#     typ = NIM_ANY
#   result = typed(node, typ)

# proc compileIndex(compiler: var Compiler, node: var Node, env: var Env): Node =
#   if len(node.children) == 1:
#     result = compiler.compileNode(node[0], env)
#   else:
#     warn node

# proc compileRaise(compiler: var Compiler, node: var Node, env: var Env): Node =
#   var exception: Node
#   var arg: Node
#   if node[0].kind == PyCall:
#     if node[0][0].kind == PyLabel:
#       exception = compiler.compileException(node[0][0].label, env)
#     else:
#       exception = node[0][0]
#     if len(node[0][1].children) > 0:
#       arg = node[0][1][0]
#     else:
#       arg = pyString("")
#   else:
#     exception = Node(kind: PyLabel, label: "Exception")
#     arg = node[0]
#   result = Node(kind: PyRaise, children: @[call(Node(kind: PyLabel, label: "newException"), @[exception, arg], T.Void)], typ: T.Void)

# proc compileAugAssign(compiler: var Compiler, node: var Node, env: var Env): Node =
#   var operator = case node[1].kind:
#     of PyAdd: "+="
#     of PySub: "-="
#     of PyMult: "*="
#     of PyFloorDiv: "div"
#     else: "?"
#   var left = compiler.compileNode(node[0], env)
#   var right = compiler.compileNode(node[2], env)
#   if operator[^1] != '=':
#     result = assign(left, binop(left, Node(kind: PyLabel, label: operator), right))
#   else:
#     result = Node(kind: NimInfix, children: @[Node(kind: PyLabel, label: operator), left, right], typ: T.Void)

# proc compileBytes(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = typed(node, T.Bytes)

# proc compileYield(compiler: var Compiler, node: var Node, env: var Env): Node =
#   node[0] = compiler.compileNode(node[0], env)
#   result = node
#   env.hasYield = true

# proc compileBreak(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = node

# proc parseOp(node: Node): string =
#   case node.kind:
#   of PyLabel:
#     result = node.label
#   of PyAnd:
#     result = "and"
#   of PyOr:
#     result = "or"
#   else:
#     result = $node.kind

# proc compileBoolOp(compiler: var Compiler, node: var Node, env: var Env): Node =
#   var z = 0
#   for child in node[1].mitems:
#     child = compiler.compileNode(child, env)
#     node[1][z] = toBool(child)
#     z += 1

#   let label = parseOp(node[0])
#   result = node[1][0]
#   for z in 1..<len(node[1].children):
#     var right = node[1][z]
#     result = binop(result, operator(label), right, typ=T.Bool)

# proc compileTuple(compiler: var Compiler, node: var Node, env: var Env): Node =
#   for z, child in node.nitems:
#     node[z] = compiler.compileNode(child, env)
#   node.typ = Type(kind: N.Tuple, elements: node.children.mapIt(it.typ))
#   result = node

# proc compileSlice(compiler: var Compiler, node: var Node, env: var Env): Node =
#   # a[start:finish]
#   #
#   # becomes
#   #
#   # a[start ..< finish]
#   #
#   # a[start:]
#   #
#   # becomes
#   #
#   # a[start .. ^1]
#   #
#   # a[:finish]
#   #
#   # becomes
#   #
#   # a[0 ..< finish]
#   #
#   # -index becomes ^index
#   var start = compiler.compileNode(node[0], env)
#   var finish = compiler.compileNode(node[1], env)
#   var a = "..<"
#   if start == PY_NIL:
#     start = pyInt(0)
#   elif start.kind == PyInt and start.i < 0:
#     start = Node(kind: NimPrefix, children: @[label("^"), start])

#   if finish == PY_NIL:
#     a = ".."
#     finish = Node(kind: NimPrefix, children: @[label("^"), pyInt(1)])
#   elif finish.kind == PyUnaryOp and finish.children[0].kind == PyUSub:
#     a = ".."
#     let limit = pyInt(finish.children[1].i+1)
#     finish = Node(kind: NimPrefix, children: @[label("^"), limit])
#   elif finish.kind == PyInt and finish.i < 0:
#     finish = Node(kind: NimPrefix, children: @[label("^"), finish])

#   result = Node(kind: NimInfix, children: @[label(a), start, finish])

# proc compileDelete(compiler: var Compiler, node: var Node, env: var Env): Node =
#   if node[0].kind != PySubscript:
#     return node
#   var sequence = compiler.compileNode(node[0][0], env)
#   var element = compiler.compileNode(node[0][1], env)
#   result = call(Node(kind: PyLabel, label: "del"), @[sequence, element], T.Void)

# proc compileUnaryOp(compiler: var Compiler, node: var Node, env: var Env): Node =
#   node[1] = compiler.compileNode(node[1], env)
#   var typ = case node[0].kind:
#     of PyNot: T.Bool
#     else: node[1].typ
#   result = typed(node, typ)

# proc compileIn(compiler: var Compiler, node: var Node, env: var Env): Node =
#   log node

# proc compileNode*(compiler: var Compiler, node: var Node, env: var Env): Node =
#   # TODO: write a macro
#   log fmt"{repeat(' ', compiler.depth)}compile {node.kind}"
#   try:
#     if node.ready:
#       result = node
#       return
#     compiler.depth += 1
#     case node.kind:
#     of PyImport:
#       result = compiler.compileImport(node, env)
#     of PyImportFrom:
#       result = compiler.compileImportFrom(node, env)
#     of PyAssign:
#       result = compiler.compileAssign(node, env)
#     of PyCall:
#       result = compiler.compileCall(node, env)
#     of PyLabel:
#       result = compiler.compileLabel(node, env)
#     of PyStr:
#       result = compiler.compileStr(node, env)
#     of PyInt:
#       result = compiler.compileInt(node, env)
#     of PyHugeInt:
#       result = compiler.compileHugeInt(node, env)
#     of PyFloat:
#       result = compiler.compileFloat(node, env)
#     of PyConstant:
#       result = compiler.compileConstant(node, env)
#     of PyExpr:
#       result = compiler.compileExpr(node, env)
#     of PyBinOp:
#       result = compiler.compileBinOp(node, env)
#     of PyFunctionDef:
#       result = compiler.compileFunctionDef(node, env)
#     of PyAttribute:
#       result = compiler.compileAttribute(node, env)
#     of Sequence:
#       result = compiler.compileSequence(node, env)
#     of PyList:
#       result = compiler.compileList(node, env)
#     of PyReturn:
#       result = compiler.compileReturn(node, env)
#     of PyIf:
#       result = compiler.compileIf(node, env)
#     of PyCompare:
#       result = compiler.compileCompare(node, env)
#     of PyClassDef:
#       result = compiler.compileClassDef(node, env)
#     of PyPass:
#       result = PY_NIL
#     of PyNone:
#       result = PY_NIL
#     of PyWith:
#       result = compiler.compileWith(node, env)
#     of PyFor:
#       result = compiler.compileFor(node, env)
#     of PyDict:
#       result = compiler.compileDict(node, env)
#     of PyWhile:
#       result = compiler.compileWhile(node, env)
#     of PyListComp:
#       result = compiler.compileListComp(node, env)
#     of PyGeneratorExp:
#       result = compiler.compileGeneratorExp(node, env)
#     of PyDictComp:
#       result = compiler.compileDictComp(node, env)
#     of PyTry:
#       result = compiler.compileTry(node, env)
#     of PySubscript:
#       result = compiler.compileSubscript(node, env)
#     of PyIndex:
#       result = compiler.compileIndex(node, env)
#     of PyRaise:
#       result = compiler.compileRaise(node, env)
#     of PyAugAssign:
#       result = compiler.compileAugAssign(node, env)
#     of PyBytes:
#       result = compiler.compileBytes(node, env)
#     of PyYield:
#       result = compiler.compileYield(node, env)
#     of PyBreak:
#       result = compiler.compileBreak(node, env)
#     of PyBoolOp:
#       result = compiler.compileBoolOp(node, env)
#     of PyTuple:
#       result = compiler.compileTuple(node, env)
#     of PySlice:
#       result = compiler.compileSlice(node, env)
#     of PyDelete:
#       result = compiler.compileDelete(node, env)
#     of PyUnaryOp:
#       result = compiler.compileUnaryOp(node, env)
#     of PyIn:
#       result = compiler.compileIn(node, env)
#     else:
#       result = PY_NIL
#       warn($node.kind)
#       # fail($node.kind)
#     if compiler.depth > 0:
#       compiler.depth -= 1
#   except Exception:
#     warn(fmt"compile {getCurrentExceptionMsg()}")
#     result = PY_NIL
#     # raise getCurrentException()
#   if result.typ.isNil:
#     result.typ = NIM_ANY

# proc compileAst*(compiler: var Compiler, file: string) =
#   var node = compiler.asts[file]
#   compiler.path = file
#   assert(not node.isNil)
#   if node.kind == PyModule:
#     compiler.modules[file] = compiler.compileModule(file, node)
#   compiler.maybeModules.del(file)

# proc loadNamespace*(compiler: Compiler, path: string): string =
#   assert path[^3..^1] == ".py" and path.startsWith(compiler.db.projectDir)
#   let tokens = path[len(compiler.db.projectDir)..^1].split("/")
#   var t = tokens.join(".")[0..^4]
#   return fmt"{compiler.db.package}{t}"

# proc mergeFunctionTypeInfo(compiler: var Compiler, node: var Node, env: var Env): Node =
#   assert node.kind == PyFunctionDef

#   var f = node.children[0]
#   assert f.kind == PyStr # TODO: label
#   var label = f.s

#   var typ: Type
#   var fullName = if compiler.currentClass.isNil: fmt"{compiler.currentModule}#{label}" else: fmt"{compiler.currentModule}.{compiler.currentClass.label}#{label}"
#   for name, t in compiler.db.types:
#     if name == fullName:
#       typ = t
#       break
#   if typ.isNil:
#     fullName = fmt"{compiler.currentModule}#{label}"
#     for name, t in compiler.db.types:
#       if name == fullName:
#         typ = t
#     if typ.isNil:
#       warn(fmt"no type for {fullName}")
#       return node

#   assert typ.kind in {N.Overloads, N.Function}

#   typ.label = label
#   env[label] = typ
#   result = node
#   result.typ = typ

# proc mergeClassTypeInfo(compiler: var Compiler, node: var Node, env: var Env): Node =
#   assert node[0].kind == PyStr
#   let label = node[0].s
#   let fullName = fmt"{compiler.currentModule}.{label}"
#   var typ: Type
#   for name, t in compiler.db.types:
#     if not t.isNil and t.kind == N.Record and name == fullName:
#       typ = t
#       typ.fullLabel = fullName
#       break

#   if typ.isNil:
#     typ = Type(kind: N.Record, init: "", label: label, fullLabel: fullName, members: initTable[string, Type]())
#   env[label] = typ

#   result = node
#   result.typ = typ

# proc mergeCallTypeInfo(compiler: var Compiler, node: var Node, env: var Env): Node =
#   result = node

# proc mergeModuleTypeInfo(compiler: var Compiler, node: var Node, env: var Env): Node =
#   if node.isNil:
#     return
#   case node.kind:
#   of PyImportFrom:
#     result = compiler.compileImportFrom(node, env)
#     result.ready = true
#   of PyFunctionDef:
#     result = compiler.mergeFunctionTypeInfo(node, env)
#   of PyClassDef:
#     result = compiler.mergeClassTypeInfo(node, env)
#   of PyCall:
#     result = compiler.mergeCallTypeInfo(node, env)
#   else:
#     var z = 0
#     for child in node.mitems:
#       node[z] = compiler.mergeModuleTypeInfo(child, env)
#       z += 1
#     result = node

# proc compile*(compiler: var Compiler, untilPass: Pass = Pass.Generation, onlyModule: string = "") =
#   var firstPath = compiler.db.startPath()
#   var node = compiler.db.loadAst(firstPath)
#   compiler.maybeModules = {firstPath: true}.toTable()
#   compiler.constants = initTable[string, Type]()
#   compiler.asts = {firstPath: node}.toTable()
#   compiler.modules = initTable[string, Module]()
#   compiler.generated = initTable[string, string]()
#   compiler.base = firstPath.rsplit("/", 1)[0]
#   compiler.identifierCollisions = initTable[string, (string, bool)]()
#   for z, path in compiler.db.modules:
#     if path.startsWith(compiler.db.projectDir) and (path.endsWith("constants.py") or path.endsWith(onlyModule)):
#       try:
#         if not compiler.modules.hasKey(path):
#           if not compiler.asts.hasKey(path):
#             compiler.asts[path] = compiler.db.loadAst(path)
#           compiler.currentModule = compiler.loadNamespace(path)
#           compiler.modules[path] = Module(name: compiler.currentModule, imports: @[], types: @[], functions: @[], init: @[])
#           compiler.stack = @[]
#           compiler.envs = {path: childEnv(nil, "", compiler.constants, nil)}.toTable()
#           compiler.currentFunction = ""
#           var node = compiler.asts[path]
#           var env = compiler.envs[path]
#           compiler.asts[path] = compiler.mergeModuleTypeInfo(node, env)
#           compiler.compileAst(path)
#       except Exception:
#         log getCurrentExceptionMsg()
#         # raise getCurrentException()
#   if untilPass == Pass.Generation:
#     var identifierCollisions = initSet[string]()
#     for label, collision in compiler.identifierCollisions:
#       if collision[1]:
#         identifierCollisions.incl(label)
#     var generator = Generator(indent: 2, v: generator.NimVersion.Development, identifierCollisions: identifierCollisions)
#     for path, module in compiler.modules:
#       compiler.generated[path] = generator.generate(module)

# when false:
#   proc compileToAst*(source: string): Node =
#     var compiler = Compiler()
#     var (types, sysPath) = traceTemp("temp.py", source)
#     var node = loadAst("temp.py")
#     compiler.compile(node, types, "temp.py", sysPath, untilPass = Pass.AST)
#     result = compiler.asts["temp.py"]


#   proc compile*(source: string): string =
#     var compiler = Compiler()
#     var (types, sysPath) = traceTemp("temp.py", source)
#     var node = loadAst("temp.py")
#     compiler.compile(node, types, "temp.py", sysPath)
#     result = compiler.generated["temp.py"]
