import strutils, strformat

proc expandTabs*(text: string, tabsize: int = 8): string =
  var column = 0
  result = ""
  for c in text:
    if column mod tabsize == 0:
      column = 0
    if c in NewLines:
      result.add(c)
      column = 0
    elif c == '\t':
      result.add(' '.repeat(tabsize - column))
      column = 0
    else:
      result.add(c)
      column += 1

# echo "01\t012\t0123\t01234".expandTabs()
# echo "01\t012\t0123\t01234".expandTabs(4)

proc findOrRaise*(text: string, sub: string, start: int = 0, last: int = 0): int =
  result = text.find(sub, start, last)
  if result == -1:
    raise newException(ValueError, fmt"{text} missing")
