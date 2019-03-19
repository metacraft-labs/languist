var suiteLabel = ""

rewrite do (x: Any, y: Class, z: Method):
  x.describe(y, z)
do:
  code:
    suiteLabel = args["y"].label
    command(variable("suite"), Node(kind: String, text: args["y"].label), Node(kind: Code, children: args["z"].code), VoidType)

rewrite do (x: Any, y: Method):
  subject(x, y)
do:
  code:
    assign(variable(args["x"].text), Node(kind: Call, children: @[variable(suiteLabel)], typ: Type(kind: T.Simple, label: suiteLabel)), Var)


rewrite "RuboCop::AST::*", Type(kind: T.Simple, label: "Node")

rewrite do (a: String, b: Method):
  it(a, b)
do:
  code:
    Node(kind: MacroCall, children: @[variable("test"), args["a"], Node(kind: Code, children: args["b"].code)], typ: VoidType)

rewrite do (x: String, y: String):
  def_node_matcher(x, y)
do:
  code:
    var res = Node(kind: MacroCall, children: @[variable("nodeMatcher"), variableGenBlock(args["x"].text), args["y"]], typ: VoidType)
    edump rewrites[1].genBlock
    res