import os, strformat, strutils

# langcop block_length metrics
let arg = paramStr(1)
let category = paramStr(2)

var directory = ""
if arg.endsWith("/"):
  directory = getHomeDir() / "rubocop" / "lib" / "rubocop" / "cop" / arg

if directory.len == 0:
  discard execShellCmd(&"./rb2nim {arg} ~/nim-rubocop \"bash ~/spec.sh {arg} {category}\"")
else:
  for child in walkDir(directory, true):
    echo child.path
    discard execShellCmd(&"./rb2nim {child.path} ~/nim-rubocop \"bash ~/spec.sh {child.path} {category}\" > /dev/null")

