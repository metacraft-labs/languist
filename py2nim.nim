import os, strformat, strutils, sequtils, tables, macros, parseopt2, helpers, osproc
import compiler, ast_parser, generator, types, tracer, trace_db

proc writeHelp =
  echo """
py2nim [-o --output <outputdir>] [-d --debug] [-a --ast] [-n --no-trace] [-m --module <module>] [-h --help] <test.py> args

--output:<outputdir>   specifies an output dir
--ast                  generates ast repr
--no-trace             starts based on the current python-deduckt.json
--module:<filename>    compiles only this module
--debug                debug stuff
--help                 shows this message

examples:

py2nim -o:a ~/a/test.py
# generates in a

py2nim -n -o:a ~/a/test.py
# starts based on the current python-deduckt.json

py2nim -m:a ~/a/test.py
# translates only a.py
"""

  quit(1)

proc save(compiler: Compiler, output: string, untilPass: Pass) =
  discard existsOrCreateDir(output)
  var folders = initTable[string, bool]()
  if untilPass == Pass.AST:
    # save a repr of ast
    for file, module in compiler.modules:
      let filename = file.rsplit("/", 1)[1].split(".")[0]
      writeFile(fmt"{output}/{filename}.nim", $module)
  else:
    for file, generated in compiler.generated:
      let tokens = file.rsplit("/", 1)
      let filename = tokens[1].split(".")[0]
      if tokens[0].startsWith(compiler.db.projectDir):
        var folder = tokens[0][len(compiler.db.projectDir)..^1]
        if len(folder) > 0 and folder[0] == '/':
          folder = folder[1..^1]
        if not folders.hasKey(folder):
          folders[folder] = true
          var exit = execCmd(fmt"mkdir -p {output}/{folder}/")
          if exit != 0:
            echo fmt"can't create {folder}"
        var expanded = expandFilename(fmt"{output}/{folder}/") & fmt"/{filename}.nim"
        success(fmt"compiled to Nim: {expanded}")
        writeFile(fmt"{output}/{folder}/{filename}.nim", generated)

proc translate =
  var command = ""
  var untilPass = Pass.Generation
  var output = "output"
  var nimArgs: seq[string] = @[]
  var pythonArgs: seq[string] = @[]
  var inPython = false
  var noTrace = false
  var onlyModule = ""
  for z in 1..paramCount():
    var arg = paramStr(z)
    if not inPython and not (":" notin arg and arg.endsWith(".py")):
      nimArgs.add(arg)
    else:
      inPython = true
      pythonArgs.add(arg)
  if len(pythonArgs) == 0:
    writeHelp()
  try:
    pythonArgs[0] = expandFilename(pythonArgs[0])
  except OSError:
    echo getCurrentExceptionMsg()
    quit(1)
  command = pythonArgs.join(" ")
  var parser = initOptParser(nimArgs.join(" "))
  for kind, key, arg in parser.getopt():
    case kind:
    of cmdLongOption, cmdShortOption:
      case key:
      of "output", "o": output = arg
      of "debug", "d": debug = true
      of "ast", "a": untilPass = Pass.AST
      of "no-trace", "n": noTrace = true
      of "module", "m": onlyModule = arg
      of "help", "h": writeHelp()
    else:
      discard

  if command == "":
    writeHelp()

  if not onlyModule.endsWith(".py"):
    onlyModule = fmt"{onlyModule}.py"

  # trace it and collect types
  if not noTrace:
    tracePython(command)
  var db = deduckt_db.load("python-deduckt.json")

  # convert to idiomatic nim
  var compiler = newCompiler(db, command)
  compiler.compile(untilPass, onlyModule)

  save(compiler, output, untilPass)
  
translate()
