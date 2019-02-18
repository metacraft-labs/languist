# old util I use for nim
import compiler/idents, compiler/ast, compiler/msgs
import strutils, sequtils

proc `$`(n: TIdent): string
proc `$`(n: TLineInfo): string
proc `$`(n: TSym): string
proc `$`(n: TInstantiation): string
proc `$`(n: PIdent): string
proc `$`(n: PSym): string
proc `$`(n: PInstantiation): string
proc `$`(n: PType): string
proc `$`(n: PNode): string

proc text*(n: TNode, depth: int = 0): string

proc text*(n: PNode, depth: int = 0): string =
  if n == nil:
    result = "$1nil" % [repeat("  ", depth)]
  else:
    result = text(n[], depth)

proc text*(n: TNode, depth: int = 0): string =
  # indent nodes
  var sons: string
  var indent = repeat("  ", depth)
  case n.kind:
  of nkCharLit..nkUInt64Lit:
    return "$1TNode($2, $3)" % [indent, $n.kind, $n.intVal]
  of nkFloatLit..nkFloat128Lit:
    return "$1TNode($2, $3)" % [indent, $n.kind, $n.floatVal]
  of nkStrLit..nkTripleStrLit:
    return "$1TNode($2, $3)" % [indent, $n.kind, n.strVal]
  of nkSym:
    sons = $n.sym[]
  of nkIdent:
    return "$1TNode(Ident, \"$2\")" % [indent, $n.ident[].s]
  else:
    sons = "@[\n$1\n$2]" % [
      n.sons.mapIt(text(it, depth + 1)).join("\n"),
      repeat("  ", depth)
    ]

  result = "$1TNode($2, sons: $3, typ: $4, info: $5)" % [indent, $n.kind, sons, $n.typ, $n.info]

proc `$`(n: TLineInfo): string =
  result = "(line: $1, col: $2)" % [$n.line, $n.col]

proc `$`(n: TLoc): string =
  result = "TLoc(k: $1)" % [$n.k]

proc `$`(n: TIdent): string =
  result = "TIdent(\"$1\")" % [$n.s]

proc `$`(n: TSym): string =
  var m = ""
  if n.kind == skModule:
    m.add(", tab seq: @[$1], usedGenerics: @[$2]" % [n.tab.data.mapIt(if it == nil: "nil" else: "PSym(kind: $1, name: $2)" % [$it.kind, $it.name]).join(","), n.usedGenerics.mapIt($it).join(",")])
    m.add(", typ: $1, ast: $2" % [$n.typ, $n.ast])
  result = "TSym(kind: $1, name: $2$3)" % [$n.kind, $n.name, m]

proc `$`(n: TNode): string =
  result = text(n, 0)

proc `$`(n: PType): string =
  if n == nil:
    result = "nil"
  else:
    result = "PType(kind: $1, callConv: $2, loc: $3)" % [$n.kind, $n.callConv, $n.loc]

proc `$`(n: PSym): string =
  if n == nil:
    result = "nil"
  else:
    result = "PSym($1" % [($n[])[5..^1]]

proc `$`(n: PNode): string =
  if n == nil:
    result = "nil"
  else:
    result = "PNode($1" % [($n[])[6..^1]]

proc `$`(n: PIdent): string =
  if n == nil:
    result = "nil"
  else:
    result = "PIdent($1" % [($n[])[7..^1]]

proc `$`(n: TInstantiation): string =
  result = "TInstantiation(sym: $1)" % [$n.sym]

proc `$`(n: PInstantiation): string =
  if n == nil:
    result = "nil"
  else:
    result = "PInstantiation($1" % [($n[])[15..^1]]

