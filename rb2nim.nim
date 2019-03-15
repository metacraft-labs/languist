import types, compiler, os, strformat, strutils, json

# rb2nim <filename pattern> <target_folder>
if paramCount() != 1 and paramCount() != 3:
  echo "rb2nim test \n" & 
       "rb2nim <filename pattern> <target_folder> <command> / <file>" 
  quit(0)

var filename = paramStr(1)
var targetFolder = ""
var command = ""


if paramCount() == 1:
  if filename == "test":
    # all files in test
    # run the single test
    # rewriting the same lang_traces.json
    var config = parseJson(readFile("test.json")).to(Config)

    for file in walkDir("test", true):
      if file.path.endswith(".rb"):
        targetFolder = "test"
        filename = file.path.splitFile()[1]
        if filename in @["class", "love"]:
          continue
        echo file.path
        let deduckt_exe = getHomedir() / "ruby-deduckt" / "exe" / "ruby-deduckt"
        command = &"ruby {deduckt_exe} -m {filename} -o {targetFolder} test/{filename}.rb"
        debug = false
        discard execShellCmd(&"{command} > /dev/null 2>&1")
        var traceDB = load(targetFolder / "lang_traces.json", rewriteinputruby, targetFolder, config)
        compile(traceDB)
        discard execShellCmd(&"nim c test/{filename}.nim > /dev/null 2>&1")
        discard execShellCmd(&"ruby test/{filename}.rb > test/ruby")
        discard execShellCmd(&"test/{filename} > test/nim")
        if readFile("test/ruby") == readFile("test/ruby"):
          echo "OK"
        else:
          echo "ERROR"

        # break # TODO
    quit(0)
  else:
    let deduckt_exe = getHomedir() / "ruby-deduckt" / "exe" / "ruby-deduckt"
    targetFolder = filename.splitFile()[0]
    let module_pattern = filename.splitFile()[1]
    command = &"env DEDUCKT_MODULE_PATTERNS={module_pattern} DEDUCKT_OUTPUT_DIR={targetFolder} bundle exec {deduckt_exe} {filename}"
    echo command
    discard execShellCmd(command)
else:
  targetFolder = expandFilename(paramStr(2))
  command = paramStr(3)
  let deduckt_exe = getHomedir() / "ruby-deduckt" / "exe" / "ruby-deduckt"
  echo &"env DEDUCKT_MODULE_PATTERNS={filename} DEDUCKT_OUTPUT_DIR={targetFolder} {command}"
  discard execShellCmd(&"env DEDUCKT_MODULE_PATTERNS={filename} DEDUCKT_OUTPUT_DIR={targetFolder} {command}")

let path = getEnv("RB2NIM_CONFIG", "")
var config = Config(imports: @[], indent: 2, name: "default config")
if path.len > 0:
  config = parseJson(readFile(path)).to(Config)

var traceDB = load(targetFolder / "lang_traces.json", rewriteinputruby, targetFolder, config)

compile(traceDB)
