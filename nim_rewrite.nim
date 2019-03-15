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

