import os, strformat

# langcop block_length metrics
let arg = paramStr(1)
let category = paramStr(2)

discard execShellCmd(&"./rb2nim {arg} ~/nim-rubocop \"bash ~/spec.sh {arg} {category}\"")

