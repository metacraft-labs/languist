import types, macros, strutils, sequtils

template pyNone*: untyped =
  Node(kind: PyNone)

proc expandLiteral(node: var NimNode): NimNode =
  if node.kind == nnkObjConstr and $(node[0]) == "Node":
    return node
  case node.kind:
  of nnkCharLit:
    result = quote:
      Node(kind: Char, c: ' ')
  of nnkIntLit..nnkUInt64Lit:
    result = quote:
      Node(kind: Int, i: `node`)
  of nnkFloatLit..nnkFloat128Lit:
    result = quote:
      Node(kind: Float, f: `node`)
  of nnkStrLit..nnkTripleStrLit:
    result = node
      
  of nnkSym, nnkIdent:
    result = node
  else:
    var z = 0
    result = node
    for child in node:
      var c = child
      result[z] = c.expandLiteral()
      z += 1

template raw*(label: untyped): untyped =
  Node(kind: Raw, label: `label`)

macro attribute*(label: static[string]): untyped =
  let fields = label.split(".")
  let base = newLit(fields[0])
  let field = newLit(fields[1])
  result = quote:
    Node(
      kind: Attribute,
      children: @[
        Node(kind: PyLabel, label: `base`, isFinished: true),
        Node(kind: String, text: `field`, isFinished: true)],
      isFinished: true)

macro attribute*(base: untyped, attr: untyped, typ: untyped = nil): untyped =
  var baseL = base
  baseL = baseL.expandLiteral()
  let t = if typ.isNil: newNilLit() else: typ
  result = quote:
    Node(
      kind: Attribute,
      typ: `t`,
      children: @[
        `baseL`,
        Node(kind: String, text: `attr`)],
      isFinished: true)

macro sequence*(args: varargs[untyped]): untyped =
  var elements = quote:
    @[`args`]
  var z = 0
  for element in elements:
    var e = element
    elements[z] = e.expandLiteral()
    z += 1
  result = quote:
    Node(
      kind: Sequence,
      children: `elements`,
      isFinished: true)

macro list*(args: varargs[untyped]): untyped =
  var elements = quote:
    @[`args`]
  var z = 0
  for element in elements:
    var e = element
    elements[z] = e.expandLiteral()
    z += 1
  result = quote:
    Node(
      kind: PyList,
      children: `elements`)

macro assign*(target: untyped, value: untyped, declaration: untyped = nil): untyped =
  var v = value
  v = v.expandLiteral()
  var dec = if declaration == nil: nnkDotExpr.newTree(ident("Declaration"), ident("Existing")) else: declaration
  result = quote:
    Node(
      kind: Assign,
      declaration: `dec`,
      children: @[
        `target`,
        `v`],
      isFinished: true)

macro pyVarDef*(target: untyped, value: untyped): untyped =
  var v = value
  v = v.expandLiteral()
  result = quote:
    Node(
      kind: PyAssign,
      children: @[
        Node(kind: Sequence, children: @[`target`]),
        `v`],
      isFinished: true)

macro variable*(name: untyped, typ: untyped = nil): untyped =
  let t = if typ.isNil: newNilLit() else: typ
  result = quote:
    Node(
      kind: Variable,
      label: `name`,
      typ: `t`,
      isFinished: true)

macro variableGenBlock*(name: untyped, typ: untyped = nil): untyped =
  let t = if typ.isNil: newNilLit() else: typ
  result = quote:
    block:
      var n = `name`
      if n.endsWith("?"):
        n = "is_" & n[0 .. ^2]
      rewrites[0].genBlock.add(n)
      rewrites[1].genBlock.add(n)
      Node(
        kind: Variable,
        label: n,
        typ: `t`,
        genBlock: true,
        isFinished: true)

macro input_variable*(name: untyped, typ: untyped = nil): untyped =
  let t = if typ.isNil: newNilLit() else: typ
  result = quote:
    Node(
      kind: Variable,
      label: `name`,
      typ: `t`)

macro input_send*(receiver: untyped, call: untyped, args: untyped, typ: untyped): untyped =
  var children = args
  var z = 0
  if children.kind == nnkPrefix:
    for child in children[1]:
      var c = child
      children[1][z] = c.expandLiteral()
      z += 1
  if children.kind != nnkPrefix:
    children = nnkPrefix.newTree(ident("@"), nnkBracket.newTree(children))
  let t = if typ.isNil: newNilLit() else: typ
  result = quote:
    Node(kind: Send, children: @[`receiver`, Node(kind: String, label: `call`)].concat(`children`), typ: `t`)
  
macro input_send*(receiver: untyped, call: untyped, typ: untyped): untyped =
  var children = nnkPrefix.newTree(ident("@"), nnkBracket.newTree())
  let t = if typ.isNil: newNilLit() else: typ
  result = quote:
    Node(kind: Send, children: @[`receiver`, Node(kind: String, text: `call`)], typ: `t`)

macro send*(receiver: untyped, call: untyped, args: varargs[untyped], typ: untyped = nil): untyped =
  var children = nnkPrefix.newTree(ident("@"), nnkBracket.newTree())
  for child in args:
    children[1].add(child)
  children = quote do: cast[seq[Node]](`children`)

  let t = if typ.isNil: newNilLit() else: typ
  if args.len > 0:
    result = quote:
      Node(kind: Send, children: @[`receiver`, Node(kind: String, text: `call`)].concat(`children`), typ: `t`, isFinished: true)
  else:
    result = quote:
      Node(kind: Send, children: @[`receiver`, Node(kind: String, text: `call`)], typ: `t`, isFinished: true)
  
# macro send*(receiver: untyped, call: untyped, typ: untyped = nil): untyped =
#   var children = nnkPrefix.newTree(ident("@"), nnkBracket.newTree())
#   let t = if typ.isNil: newNilLit() else: typ
#   echo receiver.repr
#   result = quote:
#     Node(kind: Send, children: @[`receiver`, Node(kind: String, text: `call`)], typ: `t`, isFinished: true)

macro input_call*(f: untyped, args: untyped, typ: untyped = nil): untyped =
  var children = args
  var z = 0
  if children.kind == nnkPrefix:
    for child in children[1]:
      var c = child
      children[1][z] = c.expandLiteral()
      z += 1
  if children.kind != nnkPrefix:
    children = nnkPrefix.newTree(ident("@"), nnkBracket.newTree(children))
  let t = if typ.isNil: newNilLit() else: typ
  result = quote:
    Node(kind: Call, children: @[`f`].concat(`children`), typ: `t`)

macro call*(f: untyped, typ: untyped): untyped =
  let t = typ
  result = quote:
    Node(kind: Call, children: @[`f`], typ: `t`, isFinished: true)


macro call*(f: untyped, args: untyped, typ: untyped): untyped =
  var children = args
  var z = 0
  if children.kind == nnkPrefix:
    for child in children[1]:
      var c = child
      children[1][z] = c.expandLiteral()
      z += 1
  if children.kind != nnkPrefix:
    children = nnkPrefix.newTree(ident("@"), nnkBracket.newTree(children))
  let t = typ
  echo children.repr
  result = quote:
    Node(kind: Call, children: @[`f`].concat(`children`), typ: `t`, isFinished: true)

macro call*(f: untyped, arg: untyped, arg2: untyped, typ: untyped): untyped =
  var children = arg
  children = nnkPrefix.newTree(ident("@"), nnkBracket.newTree(children))
  children[1].add(arg2)
  let t = typ
  echo children.repr
  result = quote:
    Node(kind: Call, children: @[`f`].concat(`children`), typ: `t`, isFinished: true)

macro command*(op: untyped, arg: untyped, code: untyped, t: untyped): untyped =
  result = quote:
    Node(kind: Command, children: @[`op`, `arg`, `code`], typ:  `t`, isFinished: true)

macro index*(arg: untyped, b: untyped): untyped =
  var i = b
  i = i.expandLiteral()
  result = quote:
    Node(kind: Index, children: @[`arg`, `i`], typ: `arg`.typ.args[0], isFinished: true)

template operator*(op: untyped, ignore: untyped = nil): untyped =
  Node(
    kind: Operator,
    label: `op`,
    isFinished: true)

macro compare*(op: untyped, left: untyped, right: untyped, typ: untyped): untyped =
  var (l, r) = (left, right)
  (l, r) = (l.expandLiteral(), r.expandLiteral())
  result = quote:
    Node(
      kind: BinOp,
      children: @[
        `op`,
        `l`,
        `r`],
      typ: `typ`,
      isFinished: true)

macro binop*(op: untyped, left: untyped, right: untyped, typ: untyped = nil): untyped =
  var (l, r) = (left, right)
  (l, r) = (l.expandLiteral(), r.expandLiteral())
  let t = if typ.kind == nnkNilLit: newNilLit() else: typ
  result = quote:
    Node(
      kind: BinOp,
      children: @[
        `op`,
        `l`,
        `r`],
      typ: `t`,
      isFinished: true)


macro slice*(startA: untyped, finishA: untyped = nil, stepA: untyped = nil): untyped =
  var start = startA
  start = start.expandLiteral()
  var q = quote:
    pyNone()
  var finish = if finishA.isNil: q else: finishA
  var step = if stepA.isNil: q else: stepA
  (finish, step) = (finish.expandLiteral(), step.expandLiteral())
  result = quote:
    Node(
      kind: Slice,
      children: @[
        `start`,
        `finish`,
        `step`],
      isFinished: true)

macro forrange*(label: untyped, startA: untyped, finish: untyped, code: untyped): untyped =
  var start = startA
  start = start.expandLiteral()
  result = quote:
    Node(
      kind: ForRange,
      children: @[
        `label`,
        `start`,
        `finish`,
        `code`],
      isFinished: true)

macro forin*(labels: untyped, enumerable: untyped, code: untyped): untyped =
  var labelSequence = nnkPrefix.newTree(
    ident("@"),
    nnkBracket.newTree())
  labelSequence[1].add(labels)
  result = quote:
    Node(
      kind: ForIn,
      children: `labelSequence`.concat(@[
        `enumerable`,
        `code`]),
      isFinished: true)

macro forin*(labels: untyped, labels2: untyped, enumerable: untyped, code: untyped): untyped =
  var labelSequence = nnkPrefix.newTree(
    ident("@"),
    nnkBracket.newTree())
  labelSequence[1].add(labels)
  labelSequence[1].add(labels2)
  result = quote:
    Node(
      kind: ForIn,
      children: `labelSequence`.concat(@[
        `enumerable`,
        `code`]),
      isFinished: true)

# template add*: untyped =
#   Node(kind: PyAdd, isFinished: true)

# template sub*: untyped =
#   Node(kind: PySub, isFinished: true)

# template mult*: untyped =
#   Node(kind: PyMult, isFinished: true)

# template pdiv*: untyped =
#   Node(kind: PyDiv, isFinished: true)

# template eq*: untyped =
#   Node(kind: PyEq)

# template notEq*: untyped =
#   Node(kind: PyNotEq)
