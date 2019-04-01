import os, strformat, strutils, osproc

# langcop block_length metrics
let arg = paramStr(1)
let category = paramStr(2)

var directory = ""
if arg.endsWith("/"):
  directory = getEnv("RB2NIM_RUBOCOP_PATH", getHomeDir() / "rubocop") / "lib" / "rubocop" / "cop" / arg

let nimRubocop = getEnv("RB2NIM_FAST_RUBOCOP_PATH", getHomeDir() / "nim-rubocop")

if directory.len == 0:
  echo &"./rb2nim {arg} {nimRubocop}/cops \"bash ~/spec.sh {arg} {category}\""
  discard execShellCmd(&"./rb2nim {arg} {nimRubocop}/cops \"bash ~/spec.sh {arg} {category}\"")
else:
  for child in walkDir(directory, true):
    if child.kind == pcFile:
      # Praise the Lord!
      let path = child.path.splitFile[1]
      echo path
      let status = execCmd(&"./rb2nim {path} ~/{nimRubocop}/cops \"bash ~/spec.sh {path} {category}\"")
      echo "langcop", status
      if status == 130:
        quit(1)      

