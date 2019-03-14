
import
  ruby

type
  Node* = ref object of void
    i*: int

var node = .new(0)
proc initNode*(i: void): Node =
  new(result)
  self.i = i

method `$`*(self: Node): string =
  "Node" & `$`()

echo node
