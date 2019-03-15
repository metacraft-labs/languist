
import
  ruby

import
  sequtils

var a = @[0]
a.add(1)
p(a)
p:
  var tmp = a[a.len() - 1]
  a.delete(a.len() - 1, 1)
  tmp
