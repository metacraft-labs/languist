
import
  ruby

type
  Node* = ref object of void
    i*: int

proc initNode*(i: void): void =
  new(result)
  self.i = i

method `$`*(self: Node): string =
  "Node" & self.i.toS

var node = initNode()
self.puts(node)
