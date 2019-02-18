# maybe we don't need it in the nim part anymore
# but still useful: load the python ast

import macros, strutils, sequtils, tables, helpers
import osproc, json, python_ast, python_types, gen_kind
import strformat except `%`

template log(a: string) =
  if debug:
    echo a

proc importAst*(ast: JsonNode): Node =
  if ast{"kind"} == nil:
    return nil
  var kind = ($ast{"kind"})[1..^2]
  var node: Node 
  if kind == "PyNone":
    return Node(kind: PyNone)
  for z in low(NodeKind)..high(NodeKind):
    if kind == $z:
      node = genKind(Node, z)
      break
  if node.isNil:
    log fmt"add {kind} to python_ast.nim"
  case node.kind:
  of PyStr, PyBytes:
    node.s = ($ast{"s"})[1..^2]
  of PyInt:
    node.i = parseInt($ast{"i"})
  of PyFloat:
    node.f = parseFloat($ast{"f"})
  of PyLabel:
    node.label = ($ast{"label"})[1..^2]
  elif ast{"children"} != nil:
    node.children = (ast{"children"}[]).elems.mapIt(importAst(it))
  node.line = parseInt($ast{"line"})
  node.column = parseInt($ast{"column"})
  result = node

