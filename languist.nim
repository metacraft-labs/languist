import languist/[types, compiler], os, strformat, strutils, json, osproc

# languist <filename pattern> <target_folder>
if paramCount() != 1 and paramCount() != 3:
  echo "languist test \n" & 
       "languist <filename pattern> <target_folder> <command> / <file>" 
  quit(0)

let ruby_deduckt_exe = getEnv("LANGUIST_RUBY_DEDUCKT_PATH", "") / "exe" / "ruby-deduckt"
let python_deduckt_exe = getEnv("LANGUIST_PYTHON_DEDUCKT_PATH", "") / "deduckt" / "main.py"
echo ruby_deduckt_exe
var first = paramStr(1)
var filename = first
var targetFolder = ""
var command = ""
var lang: Lang

if paramCount() == 1 and first != "last":
  if first == "test":
    # all files in test
    # run the single test
    # rewriting the same lang_traces.json
    var config = parseJson(readFile("test.json")).to(Config)

    for file in walkDir("test", true):
      if file.path.endswith(".rb") or file.path.endswith(".py"):
        targetFolder = "test"
        var extension: string
        var b: string
        (b, filename, extension) = file.path.splitFile()
        lang = getLang(extension)
        if filename in @["class", "love"]:
          continue
        case lang:
        of Lang.Ruby:
          echo &"rbenv exec bundle exec ruby {ruby_deduckt_exe} -m {filename} -o {targetFolder} test/{filename}.rb"
          command = &"rbenv exec bundle exec ruby {ruby_deduckt_exe} -m {filename} -o {targetFolder} test/{filename}.rb"
        of Lang.Python:
          command = &"env DEDUCKT_OUTPUT_DIR={targetFolder} python3 {python_deduckt_exe} test/{filename}.py"
          echo command
        debug = false
        var status = execCmd(&"{command} > /dev/null 2>&1")
        echo status
        if status == 130:
          quit(status)
        elif status != 0:
          echo "ERROR"
          continue
        var traceDB = load(targetFolder / "lang_traces.json", "", config, lang)
        compile(traceDB)
        status = execCmd(&"nim c test/{filename}.nim > /dev/null 2>&1")
        echo status
        if status == 130:
          quit(status)
        var output = ""
        case lang:
        of Lang.Ruby:
          output = "test/ruby"
          discard execCmd(&"ruby test/{filename}.rb > {output}")
        of Lang.Python:
          output = "test/python"
          discard execCmd(&"python3 test/{filename}.py > {output}")
        discard execCmd(&"test/{filename} > test/nim")
        targetFolder = ""
        if readFile("test/nim") == readFile(output):
          echo "OK"
        else:
          echo "ERROR"

        # break # TODO
    quit(0)
  else:
    var module_pattern: string
    var extension: string
    (targetFolder, module_pattern, extension) = filename.splitFile()
    lang = getLang(extension)
    case lang:
    of Lang.Ruby:
      command = &"env DEDUCKT_MODULE_PATTERNS={module_pattern} DEDUCKT_OUTPUT_DIR={targetFolder} rbenv exec bundle exec {ruby_deduckt_exe} {filename}"
    of Lang.Python:
      command = &"env DEDUCKT_MODULE_PATTERNS={module_pattern} DEDUCKT_OUTPUT_DIR={targetFolder} python3 {python_deduckt_exe} {filename}"
    echo command
    let status = execCmd(command)
    if status == 130:
      quit(status)
else:
  if first != "last":
    targetFolder = expandFilename(paramStr(2))
    command = paramStr(3)
    # echo &"env DEDUCKT_MODULE_PATTERNS={filename} DEDUCKT_OUTPUT_DIR={targetFolder} {command}"
    let status = execCmd(&"env DEDUCKT_MODULE_PATTERNS={filename} DEDUCKT_OUTPUT_DIR={targetFolder} {command}")

    if status == 130:
      quit(status)
  else:
    targetFolder = getEnv("LANGUIST_FAST_RUBOCOP_PATH") / "cops" # TODO
  lang = Lang.Ruby # TODO

var path = getEnv("LANGUIST_CONFIG", "languist.json")
if not existsFile(path):
  path = getHomeDir() / ".config" / "languist.json"
if not existsFile(path):
  path = ""
var config = Config(imports: @[], indent: 2, name: "default config")
if path.len > 0:
  config = parseJson(readFile(path)).to(Config)
echo config

var dir = targetFolder
if targetFolder == "test":
  dir = ""

var traceDB = load(targetFolder / "lang_traces.json", dir, config, lang)

compile(traceDB)
