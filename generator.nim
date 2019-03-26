import sequtils, strutils, strformat, tables, sugar, hashes, gen_kind, sets, json, macros, terminal, helpers, os, types
import
  compiler/[ast, astalgo, idents, msgs, renderer, lineinfos]

template log(a: string) =
  if helpers.debug:
    styledWriteLine(stdout, fgBlue, a, resetStyle)

template emitNode(s: untyped): untyped =
  generator.generateNode(`s`)

template ensure(k: untyped): untyped =
  assert node.kind == `k`

let nilNode = nkNilLit.newTree()
let emptyNode = newNode(nkEmpty)

macro generateIdent(s: untyped): untyped =
  result = quote:
    let label = translateIdentifier(`s`, generator.identifierCollisions)
    var res = newIdentNode(PIdent(s: label), TLineInfo())
    if label == "$":
      res = nkAccQuoted.newTree(res)
    res


macro generateDirectIdent(s: untyped): untyped =
  result = quote:
    newIdentNode(PIdent(s: `s`), TLineInfo())

proc generateNode(generator: Generator, node: Node): PNode

proc generateAssign(generator: Generator, node: Node): PNode

proc generateType(generator: Generator, typ: Type): PNode


proc generateImports(generator: Generator, imp: seq[Node]): PNode =
  assert len(imp) == 0 or imp[0].kind == Import

  var labels: seq[PNode] = @[]
  var aliases: seq[Node] = @[]
  for child in imp:
    labels = labels.concat(child.children.mapIt(generateDirectIdent(it.label)))
    aliases = aliases.concat(child.aliases)
  let top = nkImportStmt.newTree(labels)
  if len(aliases) > 0:
    let aliasNodes = aliases.mapIt(generator.generateAssign(it))
    result = nkStmtList.newTree(top)
    for alias in aliasNodes:
      result.add(alias)
  else:
    result = top

proc generateMethod(generator: Generator, met: Node): PNode

proc generateTypeDeclaration(generator: Generator, t: Node): PNode =
  assert t.kind in {Class}

  assert t.typ.kind == T.Object

  var recList: PNode
  var docstring: PNode
  let docstringText = t.docstring.join("\n")
  
  if len(t.typ.fields) > 0:
    recList = nkRecList.newTree()
    recList.add(newNode(nkEmpty))
    recList[^1].comment = docstringText
    for member, a in t.typ.fields:
      recList.add(nkIdentDefs.newTree(
        nkPostfix.newTree(generateIdent("*"), generateIdent(member)),
        generator.generateType(a),
        emptyNode))
  else:
    recList = emptyNode
    docstring = newNode(nkEmpty)
    docstring.comment = docstringText
  
  var base: PNode
  if not t.typ.base.isNil:
    base = nkOfInherit.newTree(generator.generateType(t.typ.base))
  elif t.typ.inherited:
    base = nkOfInherit.newTree(generateIdent("RootObj"))
  else:
    base = emptyNode
  var objectNode = nkObjectTy.newTree(
      emptyNode, base, recList)

  if true: # TODO why not working t.typ.isRef:
    objectNode = nkRefTy.newTree(
      objectNode)
  var tNode = nkTypeDef.newTree(
    nkPostfix.newTree(generateIdent("*"), generateIdent(t.label)),
    emptyNode,
    objectNode)

  result = nkTypeSection.newTree(tNode)
  if not docstring.isNil:
    result.add(docstring)
  for met in t.methods:
    generator.methods.add(generator.generateMethod(met.node))
  

proc generateType(generator: Generator, typ: Type): PNode =
  if typ.isNil:
    result = generateIdent("void")
  else:
    case typ.kind:
    of T.Simple, T.Object:
      var typLabel = typ.label
      if typLabel == "type":
        typLabel = "typedesc"
      elif typLabel == "bytes":
        typLabel = "cstring"
      elif typLabel in @["Int", "Void", "Bool", "String"]:
        typLabel = typLabel.toLowerAscii()
      result = generateIdent(typLabel)
    of T.Tuple:
      result = nkPar.newTree()
      for element in typ.elements:
        result.add(generator.generateType(element))
    of T.Compound:
      result = nkBracketExpr.newTree(generateIdent(typ.original.label))
      for arg in typ.args:
        result.add(generator.generateType(arg))
    of T.Generic:
      result = nkBracketExpr.newTree(generateIdent(typ.label))
      for arg in typ.genericArgs:
        result.add(generateIdent(arg))
    of T.Macro:
      result = nkCall.newTree(generateIdent(typ.label))
      for arg in typ.macroArgs:
        result.add(generator.generateType(arg))
    else:
      result = generateIdent($typ.kind)
    if typ.isVar:
      result = nkVarTy.newTree(result)

proc generateArgs(generator: Generator, nodes: seq[Node], typ: Type, returnType: Type): PNode

proc generateForward(generator: Generator, function: Node): PNode =
  assert function.kind in {NodeMethod, Block}

  eecho function.typ.isNil
  if function.returnType.isNil:
    echo "nil"
  elif function.returnType.kind == Simple:
    echo function.returnType.label
  else:
    echo function.returnType.kind
  if function.label.startsWith("on"):
    function.returnType = VoidType # HACK
  let args = generator.generateArgs(function.args, function.typ, function.returnType)

  var name = generateIdent(function.label)
  name = nkPostfix.newTree(
      generateIdent("*"),
      name)

  if function.typ.kind == T.Method:
    var genericArgs: seq[string] = @[]
    for arg in function.typ.args:
      if arg.isNil:
        continue
      case arg.kind:
      of T.Generic:
        for label in arg.genericArgs:
          if label notin genericArgs:
            genericArgs.add(label)
      of T.GenericVar:
        if arg.label notin genericArgs:
          genericArgs.add(arg.label)
      else:
        discard

    if len(genericArgs) > 0:
      name = nkBracketExpr.newTree(name)
      for arg in genericArgs:
        name.add(generateIdent(arg))

  if not function.isIterator:
    if function.kind == Block:
      result = nkLambda.newTree()
    elif function.isMethod:
      result = nkMethodDef.newTree()
    else:
      result = nkProcDef.newTree()
  else:
    result = nkIteratorDef.newTree()
  if function.kind == NodeMethod:
    result.add(name)
  else:
    result.add(emptyNode)
  result.add(emptyNode)
  result.add(emptyNode)
  result.add(args)
  result.add(emptyNode)
  result.add(emptyNode)
  result.add(emptyNode)

proc generateArgs(generator: Generator, nodes: seq[Node], typ: Type, returnType: Type): PNode =
  var iTyp = typ
  if typ.kind == T.MethodOverload:
    iTyp = typ.overloads[0]
  result = nkFormalParams.newTree()
  result.add(generator.generateType(returnType))
  var z = 0
  for arg in nodes:
    var argTyp = if z < len(iTyp.args): iTyp.args[z] else: VoidType
    result.add(nkIdentDefs.newTree(
      generateIdent(arg.label),
      generator.generateType(argTyp),
      emptyNode))
    z += 1

proc generateMethod(generator: Generator, met: Node): PNode =
  assert met.kind in {NodeMethod, Block}

  result = generator.generateForward(met)

  var children = nkStmtList.newTree()
  for child in met.code:
    children.add(emitNode(child))
  result.sons[^1] = children
  if len(met.docstring) > 0:
    var docstring = newNode(nkEmpty)
    docstring.comment = met.docstring.join("\n")
    result.sons[^1] = nkStmtList.newTree(docstring, result.sons[^1])

# proc generateFunction(generator: Generator, function: Node): PNode =
#   assert function.kind in {PyFunctionDef}

#   result = generator.generateForward(function)

#   var children = emitNode(function[2])
#   if children.kind != nkStmtList:
#     children = nkStmtList.newTree(children)
#   result.sons[^1] = children
#   if len(function.doc) > 0:
#     var docstring = newNode(nkEmpty)
#     docstring.comment = function.doc.join("\n")
#     result.sons[^1] = nkStmtList.newTree(docstring, result.sons[^1])

proc generateDeclaration(generator: Generator, declaration: Declaration): string =
  let declarations: array[Declaration, string] = ["", "let ", "var ", "const "]
  result = declarations[declaration]

proc generateAssign(generator: Generator, node: Node): PNode =
  ensure(Assign)

  let name = emitNode(node[0])
  var value: PNode
  var typ: PNode
  if not node[1].isNil:
    value = emitNode(node[1])
    typ = emptyNode
  else:
    value = emptyNode
    typ = generator.generateType(node.typ)
  case node.declaration:
  of Declaration.Var:
    result = nkVarSection.newTree(nkIdentDefs.newTree(name, typ, value))
  of Declaration.Let:
    result = nkLetSection.newTree(nkIdentDefs.newTree(name, typ, value))
  of Declaration.Const:
    result = nkConstSection.newTree(nkConstDef.newTree(name, typ, value))
  of Declaration.Existing:
    result = nkAsgn.newTree(name, value)

proc generateAugOp(generator: Generator, node: Node): PNode =
  let name = emitNode(node[0])
  let value = emitNode(node[2])
  let op = generateDirectIdent(node[1].label & "=")
  result = nkInfix.newTree(op, name, value)


proc generateNew(generator: Generator, node: Node): PNode =
  if node.children.len > 1:
    let good = generateIdent("init" & node[0].label)
    result = nkCall.newTree(good)
    for arg in node.children[1 .. ^1]:
      result.add(emitNode(arg))
  else:
    result = nkCall.newTree(generateIdent(node[0].label))

proc generateVariable(generator: Generator, node: Node): PNode =
  result = generateIdent(node.label)

proc generateIf(generator: Generator, node: Node): PNode =
  # ugh python uses if(test, body, orelse=if(test, body, orelse..))
  # and nim if(elif(test, body), elif(test, body)..)
  ensure(If)

  result = nkIfStmt.newTree()

  var last = node
  while not last.isNil and last.kind == If:
    var elifBranch = nkElifBranch.newTree(
      emitNode(last[0]),
      emitNode(last[1]))
    last = if last.children.len > 2: last[2] else: nil
    if not last.isNil and len(last.children) > 0:
      while not last.isNil and last.kind == Code and len(last.children) == 1:
        last = last[0]
      if last.kind != If:
        result.add(elifBranch)
        result.add(nkElse.newTree(emitNode(last)))
        break
    result.add(elifBranch)

proc generateWhen(generator: Generator, node: Node): PNode =
  ensure(NimWhen)

  result = nkWhen.newTree(
    nkElifBranch.newTree(
      emitNode(node[0]),
      emitNode(node[1])))
  if not node[2].isNil and len(node[2].children) > 0:
    result[0].add(nkElse.newTree(emitNode(node[2])))

proc generateCase(generator: Generator, node: Node): PNode =
  result = nkCaseStmt.newTree(emitNode(node[0]))
  for child in node.children[1 .. ^1]:
    if child.kind == Of:
      result.add(nkOfBranch.newTree(emitNode(child[0]), emitNode(child[1])))
    else:
      result.add(nkElse.newTree(emitNode(child)))

proc generateGroup(generator: Generator, group: seq[Node]): PNode =
  assert group[0].kind == Assign
  result = case group[0].declaration:
    of Declaration.Var: nkVarSection.newTree()
    of Declaration.Let: nkLetSection.newTree()
    of Declaration.Const: nkConstSection.newTree()
    else: nkStmtList.newTree()
  for element in group:
    var e = element
    e.declaration = Declaration.Existing
    result.add(emitNode(e))

proc generateCode(generator: Generator, node: Node): PNode =
  ensure(Code)

  result = nkStmtList.newTree()
  var z = 0
  while z < len(node.children):
    var child = node[z]
    var group: seq[Node] = @[]
    while z < len(node.children) - 1 and not node[z + 1].isNil and
          child.kind == Assign and node[z + 1].kind == Assign and
          child.declaration != Declaration.Existing and child.declaration == node[z + 1].declaration:
      group.add(node[z])
      z += 1
    if len(group) > 0:
      group.add(node[z])
      result.add(generator.generateGroup(group))
    else:
      result.add(emitNode(child))
    z += 1

proc generateSend(generator: Generator, node: Node): PNode =
  # either config or just guessing from trace! if self or not!
  result = nkCall.newTree(
    nkDotExpr.newTree(emitNode(node[0]), generateIdent(node[1].text)))
  for arg in node.children[2 .. ^1]:
    result.add(emitNode(arg))

proc generateCall(generator: Generator, node: Node): PNode =
  if node[0].kind == Variable and node[0].label == "include":
    result = nkImportStmt.newTree()
  else:
    result = nkCall.newTree(emitNode(node[0]))
  for i, arg in node.children:
    if i > 0:
      result.add(emitNode(arg))

  if node[0].kind == Variable and node[0].label == "echo":
    result.kind = nkCommand
  if result.kind == nkImportStmt:
    generator.top.add(result)
    result = emptyNode

proc generateMacroCall(generator: Generator, node: Node): PNode =
  # similar to command
  result = nkCommand.newTree(emitNode(node[0]))
  for i, arg in node.children:
    if i > 0:
      result.add(emitNode(arg))

proc generateCommand(generator: Generator, node: Node): PNode =
  result = nkCommand.newTree(emitNode(node[0]))
  for i, arg in node.children:
    if i > 0:
      result.add(emitNode(arg))


proc generateReturn(generator: Generator, node: Node): PNode =
  ensure(Return)

  if node.children.len > 0:
    result = nkReturnStmt.newTree(emitNode(node[0]))
  else:
    result = nkReturnStmt.newTree(emptyNode)

proc generateInt(generator: Generator, node: Node): PNode =
  result = nkIntLit.newNode()
  result.intVal = node.i

proc generateFloat(generator: Generator, node: Node): PNode =
  ensure(Float)

  result = nkFloatLit.newNode()
  result.floatVal = node.f

proc generateAttribute(generator: Generator, node: Node): PNode =
  ensure(Attribute)

  assert node[1].kind == String

  result = nkDotExpr.newTree(emitNode(node[0]), generateIdent(node[1].text))

proc generateStr(generator: Generator, node: Node): PNode =
  result = nkStrLit.newNode()
  result.strVal = node.text

proc generateDocstring(generator: Generator, node: Node): PNode =
  result = nkTripleStrLit.newNode()
  result.strVal = node.text
  edump result.strVal

# let SYMBOLS* = {
#   PyAdd: "+",
#   PySub: "-",
#   PyMult: "*",
#   PyDiv: "/",
#   PyFloorDiv: "//",
#   PyPow: "**",
#   PyEq: "==",
#   PyNotEq: "!=",
#   PyLtE: "<=",
#   PyGtE: ">=",
#   PyGt: ">",
#   PyLt: "<",
#   PyAnd: "and",
#   PyOr: "or",
#   PyNot: "not",
#   PyUSub: "-",
#   PyIs: "is",
#   PyIsNot: "isnot",
#   PyIn: "in",
#   PyNotIn: "notin",
#   PyBitAnd: "and",
#   PyBitOr: "or",
#   PyMod: "mod",
# }.toTable()

proc generateOp(generator: Generator, op: Node): PNode =
  # echo op
  let s = op.label # if SYMBOLS.hasKey(op.kind): SYMBOLS[op.kind] else: op.label
  result = generateIdent(s)

proc generateBinOp(generator: Generator, node: Node): PNode =
  ensure(BinOp)

  result = nkInfix.newTree(
    generateIdent(node[0].label),
    emitNode(node[1]),
    emitNode(node[2]))

proc generateConstr(generator: Generator, node: Node): PNode =
  assert node[0].typ.kind == T.Object

  if node[0].typ.init == "":
    result = nkObjConstr.newTree(
      emitNode(node[0]))
    for z in 0..min(node[1].children.high, node[2].children.high):
      result.add(nkExprColonExpr.newTree(emitNode(node[1][z]), emitNode(node[2][z])))
  else:
    result = nkCall.newTree(
      generateIdent(node[0].typ.init))
    for child in node[2]:
      result.add(emitNode(child))

proc generateNameConstant(generator: Generator, node: Node): PNode =
  let label = node[0].label
  case label:
  of "True", "False":
    result = generateIdent(label.toLowerAscii())
  else:
    result = nilNode

proc generateForIn(generator: Generator, node: Node): PNode =
  if node[0].kind != NimTuple:
    result = nkForStmt.newTree()
    for child in node.children:
      result.add(emitNode(child))
  else:
    result = nkForStmt.newTree(
      emitNode(node[0][0]),
      emitNode(node[0][1]),
      emitNode(node[1]),
      emitNode(node[2]))

proc generateForRange(generator: Generator, node: Node): PNode =
  var code = nkStmtList.newTree()
  for child in node[3].children:
    code.add(emitNode(child))
  let rangeCode = nkInfix.newTree(generateIdent("..<"), emitNode(node[1]), emitNode(node[2]))
  result = nkForStmt.newTree(
    emitNode(node[0]),
    rangeCode,
    code)

proc generateSequence(generator: Generator, node: Node): PNode =
  result = nkPrefix.newTree(generateIdent("@"), nkBracket.newTree())
  for child in node:
    result[1].add(emitNode(child))

proc generateRangeLess(generator: Generator, node: Node): PNode =
  result = nkInfix.newTree(generateIdent("..<"), emitNode(node[0]), emitNode(node[1]))


proc generateTable(generator: Generator, node: Node): PNode =
  var dict = nkTableConstr.newTree()
  for z in 0..<len(node.children):
    dict.add(nkExprColonExpr.newTree(emitNode(node[z][0]), emitNode(node[z][1])))
  result = nkCall.newTree(nkDotExpr.newTree(dict, generateIdent("newTable")))

proc generateWhile(generator: Generator, node: Node): PNode =
  result = nkWhileStmt.newTree(emitNode(node[0]), emitNode(node[1]))

proc generatePair(generator: Generator, node: Node): PNode =
  if node[0].kind == Symbol:
    result = nkExprEqExpr.newTree(generateIdent(node[0].text
      ), emitNode(node[1]))
  else:
    result = emptyNode

proc generateUnaryOp(generator: Generator, node: Node): PNode =
  result = nkPrefix.newTree(generateIdent(node[0].label & " "), emitNode(node[1]))

proc generateExprColonExpr(generator: Generator, node: Node): PNode =
  result = nkExprColonExpr.newtree(emitNode(node[0]), emitNode(node[1]))

proc generateTry(generator: Generator, node: Node): PNode =
  result = nkTryStmt.newTree(emitNode(node[0]))
  for child in node[1]:
    result.add(emitNode(child))

proc generateIndex(generator: Generator, node: Node): PNode =
  result = nkBracketExpr.newTree(emitNode(node[0]), emitNode(node[1]))

proc generateExcept(generator: Generator, node: Node): PNode =
  result = nkExceptBranch.newTree(
    if node[0].kind != Nil: emitNode(node[0]) else: emptyNode,
    emitNode(node[1]))

proc generateRaise(generator: Generator, node: Node): PNode =
  result = nkRaiseStmt.newTree(emitNode(node[0]))

proc generateInfix(generator: Generator, node: Node): PNode =
  result = nkInfix.newTree(emitNode(node[0]), emitNode(node[1]), emitNode(node[2]))

proc generateAccQuoted(generator: Generator, node: Node): PNode =
  result = nkAccQuoted.newTree(emitNode(node[0]))

proc generateYield(generator: Generator, node: Node): PNode =
  if node.children.len > 0:
    result = nkYieldStmt.newTree(emitNode(node[0]))
  else:
    result = nkYieldStmt.newTree()

proc generateBreak(generator: Generator, node: Node): PNode =
  result = nkBreakStmt.newTree(emptyNode)

proc generateWith(generator: Generator, node: Node): PNode =
  result = nkCommand.newTree(
    generateIdent("with"),
    emitNode(node[0][0]),
    emitNode(node[1]))

proc generateOf(generator: Generator, node: Node): PNode =
  result = nkInfix.newTree(generateIdent("of"), emitNode(node[0]), emitNode(node[1]))

proc generateTuple(generator: Generator, node: Node): PNode =
  result = nkPar.newTree()
  for child in node:
    result.add(emitNode(child))

proc generatePrefix(generator: Generator, node: Node): PNode =
  result = nkPrefix.newTree(emitNode(node[0]), emitNode(node[1]))


proc generateImport(generator: Generator, node: Node): PNode =
  result = nkImportStmt.newTree()
  for child in node:
    result.add(emitNode(child))

proc generateMath(generator: Generator, op: string, node: Node): PNode =
  result = nkInfix.newTree(generateIdent(op), emitNode(node[0]), emitNode(node[1]))


proc generateSlice(generator: Generator, node: Node): PNode =
  var start = if node[0].kind != Nil: emitNode(node[0]) else: newIntNode(nkIntLit, 0)
  var finish = if node[0].kind != Nil: emitNode(node[1]) else: nkPrefix.newTree(generateIdent("^"), newIntNode(nkIntLit, 1))
  var op = if node[0].kind != Nil: "..<" else: ".."
  result = nkInfix.newTree(generateIdent(op), start, finish)


proc generateCommentedOut(generator: Generator, node: Node): PNode =
  result = newNode(nkEmpty)
  if node[0].kind == String:
    result.comment = fmt"py2nim can't generate code for{endl}{node[0].text}"

proc generateContinue(generator: Generator, node: Node): PNode =
  result = nkContinueStmt.newTree(emptyNode)

proc homogeneous(node: Node): bool =
  node.children.len == 0 or node.children.allIt(it.kind == node.children[0].kind)


proc generateAlias(generator: Generator, node: Node): PNode =
  #var met = node.deepCopy()
  #result = generator.generateForward(met)
  result = emptyNode #generator.generateFunction()

proc generateNode(generator: Generator, node: Node): PNode =
  if node.isNil:
    result = nilNode
    return
  edump node.kind
  case node.kind:
  of Assign:
    result = generator.generateAssign(node)
  of AugOp:
    result = generator.generateAugOp(node)
  of New:
    result = generator.generateNew(node)
  of Variable, RubyConst:
    result = generator.generateVariable(node)
  of Self:
    result = generateIdent("self")
  of Super:
    result = generateIdent("super") # TODO
  of If:
    result = generator.generateIf(node)
  of Comment:
    result = emptyNode
  of NimWhen:
    result = generator.generateWhen(node)
  of Code:
    result = generator.generateCode(node)
  of Case:
    result = generator.generateCase(node)
  of Call:
    result = generator.generateCall(node)
  of MacroCall:
    result = generator.generateMacroCall(node)
  of Command:
    result = generator.generateCommand(node)
  of Return:
    result = generator.generateReturn(node)
  of Int:
    result = generator.generateInt(node)
  of Bool:
    result = generateIdent($node.val)
  of Float:
    result = generator.generateFloat(node)
  of Attribute:
    result = generator.generateAttribute(node)
  of Send:
    result = generator.generateSend(node)
  of String, Symbol:
    result = generator.generateStr(node)
  of Docstring: 
    result = generator.generateDocstring(node)
  of BinOp:
    result = generator.generateBinOp(node)

  of ForIn:
    result = generator.generateForIn(node)
  of ForRange:
    result = generator.generateForRange(node)
  of Block:
    result = generator.generateMethod(node)
  of Sequence:
    if homogeneous(node):
      result = generator.generateSequence(node)
    else:
      result = generator.generateTuple(node)
  of NimRangeLess:
    result = generator.generateRangeLess(node)
  of NimTable:
    result = generator.generateTable(node)
  of While:
    result = generator.generateWhile(node)
  of Pair:
    result = generator.generatePair(node)
  of UnaryOp:
    result = generator.generateUnaryOp(node)
  of NimExprColonExpr:
    result = generator.generateExprColonExpr(node)
  of NimTuple:
    result = generator.generateTuple(node)
  of Try:
    result = generator.generateTry(node)
  of Index:
    result = generator.generateIndex(node)
  of Except:
    result = generator.generateExcept(node)
  of Raise:
    result = generator.generateRaise(node)
  of NimInfix:
    result = generator.generateInfix(node)
  of NimAccQuoted:
    result = generator.generateAccQuoted(node)
  of Yield:
    result = generator.generateYield(node)
  of Break:
    result = generator.generateBreak(node)
  of NimOf:
    result = generator.generateOf(node)
  of NimPrefix:
    result = generator.generatePrefix(node)
  of RubyAlias:
    result = generator.generateAlias(node)
  of Import:
    result = generator.generateImport(node)
  of NimSlice:
    result = generator.generateSlice(node)
  of NimCommentedOut:
    result = generator.generateCommentedOut(node)
  of Continue:
    result = generator.generateContinue(node)
  of Class:
    result = generator.generateTypeDeclaration(node)
  else:
    log fmt"? {node.kind}"
    result = emptyNode

var rewriteGenerator = initTable[string, proc(generator: Generator): PNode]()

include generator_rewrite

proc generate*(generator: Generator, module: Module, config: Config): string =
  generator.module = module
  generator.top = newNode(nkStmtList)
  generator.types = newNode(nkStmtList)
  generator.methods = newNode(nkStmtList)
  generator.global = newNode(nkStmtList)
  generator.main = newNode(nkStmtList)

  # mercy!
  echo config
  for i in config.imports:
    generator.top.add(generator.generateImport(Node(kind: Code, children: @[variable(i)])))
  if len(module.imports) > 0:
    generator.top.add(generator.generateImports(module.imports))

  for t in module.classes:


    
    generator.types.add(generator.generateTypeDeclaration(t))

  for b in module.main:
    if b.kind == Assign and b.declaration == Declaration.Const or b.kind in {MacroCall}:
      generator.global.add(emitNode(b))
    else:
      generator.main.add(emitNode(b))

  var forward: seq[Node] = @[]

  var program: PNode
  for label, function in rewriteGenerator:
    edump label
    edump module.path
    if label in module.path:
      program = rewriteGenerator[label](generator)
      break
  if program.isNil:
    program = nkStmtList.newTree(
      generator.top,
      newNode(nkEmpty),
      generator.types,
      newNode(nkEmpty),
      generator.global,
      newNode(nkEmpty),
      generator.methods,
      newNode(nkEmpty),
      generator.main)

  result = program.renderTree({renderDocComments}) & "\n"
