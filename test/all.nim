
import
  ruby

echo 0
var a = @[4, 0].isAll(proc (element: int): bool =
  element > 0)
p(a)
