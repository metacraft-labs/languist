# Praise the Lord!

import types, 
  compiler / [
    parser, idents, msgs, configuration, 
    ast, options,
    condsyms,nimconf, extccomp, pathutils],
  sequtils, strutils, strformat, tables, sets, os, ast_dsl


import compiler/types as t2
# from compiler/types import typeToString, preferDesc
include deeptext

let GENERICS = @["T", "U"].toHashSet()

proc loadType(child: PNode): Type =
  case child.kind:
  of nkIdent:
    if child.ident.s == "Any":
      Type(kind: T.Any)
    else:
      Type(kind: T.Simple, label: child.ident.s)
  of nkBracketExpr:
    if child[0].kind == nkIdent:
      if child[1].kind == nkIdent and child[1].ident.s in GENERICS:
        Type(kind: T.Generic, label: child[0].ident.s, genericArgs: @[child[1].ident.s])
      else:
        Type(kind: T.Compound, args: @[loadType(child[1])], original: Type(kind: T.Generic, label: child[0].ident.s, genericArgs: @["T"]))
    else:
      nil
  else:
    nil

proc loadSignature(child: PNode, returnType: PNode, typ: Type = nil): RewriteRule =
  result = RewriteRule()
  case child.kind:
  of nkObjConstr:
    if typ.isNil:
      result.input = Node(kind: Call, children: @[variable(child[0].ident.s)])
    else:
      result.input = Node(kind: Send, children: @[variable("self"), Node(kind: String, text: child[0].ident.s)])
    result.replacedPos = initTable[string, int]()
    for i, arg in child:
      if i > 0:
        echo "arg ", arg.e
        let typ = loadType(arg[1])
        result.input.children.add(variable(arg[0].ident.s, typ=typ))
        result.args.add(@[])
        result.replaced.add((label: arg[0].ident.s, typ: typ))
        result.replacedPos[arg[0].ident.s] = i - 1
  else:
    discard

proc loadCode(child: PNode, signature: RewriteRule): RewriteRule =
  var ch = child
  while ch.kind == nkStmtList and ch.len == 1:
    ch = child[0]
  result = signature
  # code is translated to a Node and to some state
  # which helps rewrite to later assign to the correct fields/sequence
  # the matched input
  echo ch.e
  case ch.kind:
  of nkCall:
    assert ch[0].kind == nkIdent
    result.output = Node(kind: Call)
    result.output.children = @[variable(ch[0].ident.s)]
    for i, arg in ch:
      if i > 0:
        case arg.kind:
        of nkIdent:
          echo result.replacedPos, arg.ident.s, result.replacedPos.hasKey(arg.ident.s)
          if result.replacedPos.hasKey(arg.ident.s):
            echo arg.ident.s
            result.output.children.add(nil) # similar so we can add it to replace!
            result.replaceList.add((result.replacedPos[arg.ident.s], @[i - 1]))
          else:
            result.output.children.add(variable(arg.ident.s))
        of nkCharLit..nkUInt64Lit:
          result.output.children.add(Node(kind: Int, i: arg.intVal.int))
        of nkFloatLit..nkFloat128Lit:
          result.output.children.add(Node(kind: Float, f: arg.floatVal.float))
        of nkStrLit..nkTripleStrLit:
          result.output.children.add(Node(kind: String, text: arg.strVal))
        of nkSym:
          discard
        else:
          echo "todo ", arg.e
  else:
    discard
  

proc loadMapping(child: PNode, typ: Type, dep: var seq[string], res: var Rewrite) =
  case child.kind:
  of nkCommand:
    if child[0].kind == nkIdent and child[1].kind == nkIdent and child[0].ident.s == "dep":
      dep = @[child[1].ident.s]
  of nkInfix:
    assert child[0].ident.s == "->"

    var signature = loadSignature(child[1], child[2], typ)
    var right = loadCode(child[3], signature)
    right.dependencies = dep
    echo right
    res.rules.add(right)
  else:
    discard
    
proc loadDSL*(dsl: PNode, res: var Rewrite) =
  for child in dsl:
    case child.kind:
    of nkInfix:
      # signature: right
      var dep: seq[string]
      loadMapping(child, nil, dep, res)
    of nkCall:
      if child[0].kind == nkIdent:
        if child[0].ident.s == "typ":
          # typ(T): children
          let typ = loadType(child[1])
          var dep: seq[string]
          for typChild in child[2]:
            loadMapping(typChild, typ, dep, res)

        elif child[0].ident.s == "rewrite":
          # rewrite(a: A, b: B): children
          var args: Table[string, Type]
          for arg in child[1]:
            assert arg[0].kind == nkIdent
            args[arg[0].ident.s] = loadType(arg[1])
        else:
          discard
      else:
        discard
    else:
      discard

proc loadRewrite*(package: IdiomPackage): Rewrite =
  if existsFile(cacheDir / package.id & ".nim"):
    result = Rewrite(
      rules: @[],
      types: initTable[string, Type](),
      genBlock: @[],
      symbolRules: @[],
      lastCalls: @[])
    var source = readFile(cacheDir / package.id & ".nim")
    var conf = newConfigRef()
    var cache = newIdentCache()
    condsyms.initDefines(conf.symbols)
    conf.projectName = "stdinfile"
    conf.projectFull = AbsoluteFile"stdinfile"
    conf.projectPath = canonicalizePath(conf, AbsoluteFile(getCurrentDir())).AbsoluteDir
    conf.projectIsStdin = true
    loadConfigs(DefaultConfig, cache, conf)
    extccomp.initVars(conf)
    var node = parseString(source, cache, conf)
    loadDSL(node, result)

var rewrites*: seq[Rewrite] = @[]

proc loadIdioms*(traceDB: TraceDB) =
  for package in traceDB.config.idioms:
    var rewrite = loadRewrite(package)
    if not rewrite.isNil:
      rewrites.add(rewrite)
