import os, strformat, strutils, osproc

# langcop block_length metrics
let arg = paramStr(1)
let category = paramStr(2)

var directory = ""
if arg.endsWith("/"):
  directory = getEnv("LANGUIST_RUBOCOP_PATH", getHomeDir() / "rubocop") / "lib" / "rubocop" / "cop" / arg

let nimRubocop = getEnv("LANGUIST_FAST_RUBOCOP_PATH", getHomeDir() / "nim-rubocop")
let directory2 = getEnv("LANGUIST_RUBOCOP_PATH")
  
if directory.len == 0:
  echo &"languist {arg} {nimRubocop}/cops \"bash {directory2}/spec.sh {arg} {category}\""
  discard execShellCmd(&"languist {arg} {nimRubocop}/cops \"bash {directory2}/spec.sh {arg} {category}\"")
else:
  for child in walkDir(directory, true):
    if child.kind == pcFile:
      
      # Praise the Lord!
      let path = child.path.splitFile[1]
      echo path
      let status = execCmd(&"languist {path} {nimRubocop}/cops \"bash {directory2}/spec.sh {path} {category}\"")
      echo "langcop", status
      if status == 130:
        quit(1)      

