
import
  ruby

import
  sequtils

var a = @[4, 0].anyIt:
  it > 0
p(a)
