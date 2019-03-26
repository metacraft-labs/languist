
import
  ruby

type
  E* = ref object
method e*(self: E; b: int): void =
  var c: int
  if b > 0:
    c = 0
  else:
    c = 2
  p(c)

var b = E()
b.e(0)
