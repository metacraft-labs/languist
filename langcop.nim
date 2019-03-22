import os, strformat, strutils, osproc

# langcop block_length metrics
let arg = paramStr(1)
let category = paramStr(2)

var directory = ""
if arg.endsWith("/"):
  directory = getHomeDir() / "rubocop" / "lib" / "rubocop" / "cop" / arg

if directory.len == 0:
  discard execShellCmd(&"./rb2nim {arg} ~/nim-rubocop/cops \"bash ~/spec.sh {arg} {category}\"")
else:
  for child in walkDir(directory, true):
    if child.kind == pcFile:
      # Praise the Lord!
      let path = child.path.splitFile[1]
      echo path
      let status = execCmd(&"./rb2nim {path} ~/nim-rubocop/cops \"bash ~/spec.sh {path} {category}\"")
      echo "langcop", status
      if status == 130:
        quit(1)      

