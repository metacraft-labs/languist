
import
  ruby

import
  sequtils

var a = @[4, 0].filterIt:
  it > 0
p(a)
