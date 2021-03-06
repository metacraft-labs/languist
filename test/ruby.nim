import strutils, sequtils

proc inspect[T](b: T): string =
  $b

proc inspect(b: int): string =
  $b

proc inspect(b: string): string =
  "\"" & b & "\""

proc inspect[T](b: seq[T]): string =
  "[" & b.mapIt(it.inspect).join(", ") & "]"

proc p*[T](b: T): void =
  echo b.inspect
