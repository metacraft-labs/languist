import types, compiler, os, strformat, strutils

# rb2nim <filename pattern> <target_folder>
if paramCount() != 1 and paramCount() != 3:
  echo "rb2nim test \n" & 
       "rb2nim <filename pattern> <target_folder> <command> / <file>" 
  quit(0)

var filename = paramStr(1)
var targetFolder = ""
var command = ""
echo paramCount()
if paramCount() == 1:
  if filename == "test":
    # all files in test
    # run the single test
    # rewriting the same lang_traces.json
    for file in walkDir("test", true):
      echo file.path
      if file.path.endswith(".rb"):
        targetFolder = "test"
        filename = file.path.splitFile()[1]
        command = &"ruby tracing.rb test/{filename}.rb"
        echo &"env RB2NIM_FILENAME={filename} RB2NIM_TARGET_FOLDER={targetFolder} {command}"
        discard execShellCmd(&"env RB2NIM_FILENAME={filename} RB2NIM_TARGET_FOLDER={targetFolder} RB2NIM_RUN_RUBY=true {command}")
        var traceDB = load(targetFolder / "lang_traces.json", rewriteinputruby, targetFolder)
        compile(traceDB)
        discard execShellCmd(&"nim c test/{filename}.nim > /dev/null")
        discard execShellCmd(&"ruby test/{filename}.rb > test/ruby")
        discard execShellCmd(&"test/{filename} > test/nim")
        if readFile("test/ruby") == readFile("test/ruby"):
          echo "OK"
        else:
          echo "ERROR"

        break # TODO
    quit(0)
  else:
    targetFolder = filename.splitFile()[0]
    command = &"ruby tracing.rb {filename}"
else:
  targetFolder = expandFilename(paramStr(2))
  command = paramStr(3)
echo &"env RB2NIM_FILENAME={filename} RB2NIM_TARGET_FOLDER={targetFolder} {command}"
discard execShellCmd(&"env RB2NIM_FILENAME={filename} RB2NIM_TARGET_FOLDER={targetFolder} RB2NIM_RUN_RUBY=true {command}")

var traceDB = load(targetFolder / "lang_traces.json", rewriteinputruby, targetFolder)

compile(traceDB)
