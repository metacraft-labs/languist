# producing directly code

import
  strformat, strutils, sequtils,
  module, python_ast


type
  NimVersion* {.pure.} = enum V017, Development

  Generator* = object
    indent*:    int
    v*:         NimVersion
    module*:    Module
    code*:      string


template emitTop(s: untyped): untyped =
  generator.code.add(`s`)

template emit(s: untyped): untyped =
  result.add(`s`)

template emitNode(s: untyped): untyped =
  result.add(generator.generateNode(`s`)) 

template ensure(k: untyped): untyped =
  assert node.kind == `k`
  result = ""

let endl = "\n"

proc generateNode(generator: var Generator, node: Node): string

proc generateAssign(generator: var Generator, node: Node): string

proc generateImport(generator: var Generator, imp: Node) =
  assert imp.kind == PyImport

  let labels = imp.children.mapIt(it.label).join(" ")
  emitTop fmt"import {labels}{endl}"
  for alias in imp.aliases:
    emitTop generator.generateAssign(alias)
    emitTop "\n"

proc generateClass(generator: var Generator, t: Node) =
  assert t.kind in {PyClassDef}

  emitTop fmt"type {t.label} = object{endl}"

proc generateFunction(generator: var Generator, function: Node) =
  assert function.kind in {PyFunctionDef}

  emitTop fmt"proc {function[0].s}(){endl}"

proc generateDeclaration(generator: var Generator, declaration: Declaration): string =
  let declarations: array[Declaration, string] = ["", "let ", "var ", "const "]
  result = declarations[declaration]

proc generateAssign(generator: var Generator, node: Node): string =
  ensure(PyAssign)
  
  let declaration = generator.generateDeclaration(node.declaration)

  emit declaration
  emitNode node[0][0]
  emit " = "
  emitNode node[1]

proc generateNode(generator: var Generator, node: Node): string =
  # TODO: macro
  case node.kind:
  of PyAssign:
    result = generator.generateAssign(node)
  else:
    echo "?", node.kind
    result = "?"

proc generate*(generator: var Generator, module: Module): string =
  generator.module = module
  generator.code = ""
  
  for imp in module.imports:
    generator.generateImport(imp)

  for t in module.types:
    generator.generateClass(t)

  for function in module.functions:
    generator.generateFunction(function)

  for i in module.init:
    emitTop generator.generateNode(i)
    emitTop "\n"

  result = generator.code
