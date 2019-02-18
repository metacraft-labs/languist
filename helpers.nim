import strformat, strutils, terminal, os

type
  Textable* = concept a
    $a is string

var debug* = false

proc warn*(a: Textable) =
  styledWriteLine(stderr, fgYellow, fmt"warn: {$a}", resetStyle)

proc fail*(a: Textable) =
  styledWriteLine(stderr, fgRed, fmt"error: {$a}", resetStyle)
  writeStackTrace()
  quit(1)

proc success*(a: Textable) =
  styledWriteLine(stdout, fgGreen, a, resetStyle)

proc findSource*(path: string, line: int, column: int, text: string): string =
  # echo path, line, column
  if line < 1 or column < 0:
    result = text
    return
  try:
    let source = readFile(path)
    let lines = source.splitLines()
    if not (line - 1 < len(lines)):
      result = text
    else:
      let lin = lines[line - 1]
      if not (column < len(lin)):
        result = text
      else:
        result = lin[column..^1]
  except Exception:
    echo "cant open ", path
    result = text
