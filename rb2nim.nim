import types, compiler, os, strformat

# rb2nim <filename pattern> <target_folder>
if paramCount() != 1 and paramCount() != 3:
  echo "rb2nim  <filename pattern> <target_folder> <command> / <file>" 
  quit(0)

let filename = paramStr(1)
var targetFolder = ""
var command = ""
echo paramCount()
if paramCount() == 1:
  targetFolder = filename.splitFile()[0]
  command = &"ruby tracing.rb {filename}"
else:
  targetFolder = expandFilename(paramStr(2))
  command = paramStr(3)
echo &"env RB2NIM_FILENAME={filename} RB2NIM_TARGET_FOLDER={targetFolder} {command}"
discard execShellCmd(&"env RB2NIM_FILENAME={filename} RB2NIM_TARGET_FOLDER={targetFolder} RB2NIM_RUN_RUBY=true {command}")

var traceDB = load(targetFolder / "lang_traces.json", rewriteinputruby, targetFolder)

compile(traceDB)
