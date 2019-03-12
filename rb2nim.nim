import types, compiler, os, strformat

# rb2nim <filename pattern> <target_folder>
if paramCount() < 3:
  echo "rb2nim <filename pattern> <target_folder> <command>" 
  quit(0)

let filename = paramStr(1)
let targetFolder = expandFilename(paramStr(2))
let command = paramStr(3)
discard execShellCmd(&"env RB2NIM_FILENAME={filename} RB2NIM_TARGET_FOLDER={targetFolder} {command}")

var traceDB = load(targetFolder / "lang_traces.json", rewriteinputruby, targetFolder)

compile(traceDB)
