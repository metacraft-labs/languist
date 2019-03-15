static:
  inRuby = false

rewrite do (x: Int):
  x.is_positive()
do -> Bool:
  x > 0

rewrite do (x: Any):
  echo(x)
do -> Void:
  echo(x)

# TODO
rewrite do (x: Sequence, y: Method):
  x.map(y)
do:
  code:
    send(args["x"], "mapIt", rewriteIt(args["y"]))
  dependencies: @["sequtils"]

rewrite do (x: Sequence, y: Method):
  x.filter(y)
do:
  code:
    send(args["x"], "filterIt", rewriteIt(args["y"]))
  dependencies: @["sequtils"]

rewrite do (x: Sequence, y: Method):
  x.is_all(y)
do:
  code:
    send(args["x"], "allIt", rewriteIt(args["y"]))
  dependencies: @["sequtils"]

rewrite do (x: Sequence, y: Method):
  x.is_any(y)
do:
  code:
    send(args["x"], "anyIt", rewriteIt(args["y"]))
  dependencies: @["sequtils"]


rewrite do (x: Sequence, y: Any):
  x.push(y)
do:
  code:
    send(args["x"], "add", args["y"])

rewrite do (x: Sequence):
  x.pop()
do:
  code:
    # Faith
    Node(kind: Code,
      children: @[
        assign(variable("tmp"), index(args["x"], binop(operator("-"), send(args["x"], "len"), 1)), Var),
        send(args["x"], "delete", binop(operator("-"), send(args["x"], "len"), Node(kind: Int, i: 1)), Node(kind: Int, i: 1)),
        variable("tmp")])
  dependencies: @["sequtils"]