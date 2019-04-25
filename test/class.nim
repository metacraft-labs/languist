
import
  types

type
  Node* = ref object of void
    i*: int

proc initNode*(i: void) =
  new(result)
  self. = i

method `$`*(self: Node): string =
  "Node" + self.i.toS

var node = initNode()
self.puts(node)
