static:
  inRuby = true

rewrite do (x: Any):
  puts x
do -> String:
  echo(x)


rewrite do (x: Any):
  x.to_s()
do -> String:
  $x
  
rewrite do (x: String, y: String):
  x + y
do -> String:
  x & y

rewrite do (x: Int, y: Method):
  x.times(y)
do:
  code:
    forrange(args["y"].args[0], 0, args["x"], Node(kind: Code, children: args["y"].code))

# FAITH
rewrite do (x: Table, y: Method):
  x.each(y)
do:
  code:
    if args["y"].args.len == 1:
      forin(args["y"].args[0], args["x"], Node(kind: Code, children: args["y"].code))
    else:
      forin(args["y"].args[0], args["y"].args[1], args["x"], Node(kind: Code, children: args["y"].code)) 
      # TODO matching more exact , but it doesnt really matter for now
  
  dependencies: @["tables"]

rewrite do (x: Sequence, y: Method):
  x.each(y)
do:
  code:
    forin(args["y"].args[0], args["x"], Node(kind: Block, children: args["y"].code))
