import languist/[types, compiler], os, strformat, strutils, json, osproc, yaml.serialization, streams, tables

# languist <filename pattern> <target_folder>
if paramCount() != 1 and paramCount() != 3:
  echo "languist test \n" & 
       "languist <filename pattern> <target_folder> <command> / <file>\n" &
       "languist init\n" &
       "languist install"
  quit(0)

let ruby_deduckt_exe = getEnv("LANGUIST_RUBY_DEDUCKT_PATH", "") / "exe" / "ruby-deduckt"
let python_deduckt_exe = getEnv("LANGUIST_PYTHON_DEDUCKT_PATH", "") / "deduckt" / "main.py"
echo ruby_deduckt_exe
var first = paramStr(1)
var filename = first
var targetFolder = ""
var command = ""
var lang: Lang
var repoPath = getHomeDir() / "languist_idioms"
# TODO: refactor
const DEFAULT = """
nim:
  lang: 0.19
ruby:
  lang: 2.5
python:
  lang: 3.4
"""


proc init =
  if not existsFile("idioms.yaml"):
    writeFile("idioms.yaml", DEFAULT)

proc loadIdiomList(path: string): seq[IdiomPackage] =
  var t: InputPackage # Table[string, RawPackage] # = initTable[string, RawPackage]()
  load(newFileStream(path), t)
  result = @[]
  for lang, raw in t.input:
    for name, version in raw:
      result.add(IdiomPackage(name: name, version: $version, lang: getLang(lang), isLang: name == "lang", isTarget: false))
  for lang, raw in t.target:
    for name, version in raw:
      result.add(IdiomPackage(name: name, version: $version, lang: getLang(lang), isLang: name == "lang", isTarget: true))

proc downloadRaw(package: IdiomPackage, path: string) =
  # TODO http
  let input = repoPath / package.id & ".nim"

  if existsFile(input):
    copyFile input,  path
    echo &"Installed {package.id}"
  else:
    echo &"No version {package.id}"

proc downloadIdiom(package: IdiomPackage) =
  if not existsDir(cacheDir):
    createDir cacheDir
  let path = cacheDir / package.id & ".nim"
  if not existsFile(path):
    downloadRaw(package, path)
  else:
    echo &"Found {package.id} in cache"

proc install =
  init()
  var idiomList = loadIdiomList("idioms.yaml")
  for idioms in idiomList:
    downloadIdiom(idioms)

if paramCount() == 1 and first != "last":
  if first == "test":
    # all files in test
    # run the single test
    # rewriting the same lang_traces.json
    var config = parseJson(readFile("config/test.json")).to(Config)
    config.idioms = loadIdiomList("idioms.yaml")

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
          echo &"bundle exec ruby {ruby_deduckt_exe} -m {filename} -o {targetFolder} test/{filename}.rb"
          command = &"bundle exec ruby {ruby_deduckt_exe} -m {filename} -o {targetFolder} test/{filename}.rb"
        of Lang.Python:
          command = &"env DEDUCKT_OUTPUT_DIR={targetFolder} python3 {python_deduckt_exe} test/{filename}.py"
          echo command
        else:
          discard
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
        else:
          discard
        discard execCmd(&"test/{filename} > test/nim")
        targetFolder = ""
        if readFile("test/nim") == readFile(output):
          echo "OK"
        else:
          echo "ERROR"

        # break # TODO
    quit(0)
  elif first == "init":
    init()
    quit(0)
  elif first == "install":
    install()
    quit(0)
  else:
    var module_pattern: string
    var extension: string
    (targetFolder, module_pattern, extension) = filename.splitFile()
    lang = getLang(extension)
    case lang:
    of Lang.Ruby:
      command = &"env DEDUCKT_MODULE_PATTERNS={module_pattern} DEDUCKT_OUTPUT_DIR={targetFolder} bundle exec {ruby_deduckt_exe} {filename}"
    of Lang.Python:
      command = &"env DEDUCKT_MODULE_PATTERNS={module_pattern} DEDUCKT_OUTPUT_DIR={targetFolder} python3 {python_deduckt_exe} {filename}"
    else:
      discard
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
    targetFolder = "test" # getEnv("LANGUIST_FAST_RUBOCOP_PATH") / "cops" # TODO
  lang = Lang.Ruby # TODO

var path = getEnv("LANGUIST_CONFIG", "languist.json")
if not existsFile(path):
  path = getHomeDir() / ".config" / "languist.json"
if not existsFile(path):
  path = ""
var config = Config(imports: @[], indent: 2, name: "default config")
if path.len > 0:
  config = parseJson(readFile(path)).to(Config)
config.idioms = loadIdiomList("idioms.yaml")

var dir = targetFolder
if targetFolder == "test":
  dir = ""

var traceDB = load(targetFolder / "lang_traces.json", dir, config, lang)

compile(traceDB)
