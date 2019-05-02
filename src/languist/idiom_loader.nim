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
let GENERIC_LIST = @["T", "U"]
proc loadType(child: PNode): Type =
  echo "type ", child.e
  result = case child.kind:
    of nkIdent:
      let label = child.ident.s
        
      if label in GENERICS:
        Type(kind: T.GenericVar, label: label)
      elif label == "Any":
        Type(kind: T.Any)
      else:
        Type(kind: T.Simple, label: label)

    of nkBracketExpr:
      if child[0].kind == nkIdent:
        if child.len == 2 and child[1].kind == nkIdent and child[1].ident.s in GENERICS:
          Type(kind: T.Generic, label: child[0].ident.s, genericArgs: @[child[1].ident.s])
        else:
          var res = Type(kind: T.Compound, args: @[loadType(child[1])], original: Type(kind: T.Generic, label: child[0].ident.s, genericArgs: @["T"]))
          if child.len > 2:
            for i, ch in child:
              if i > 1:
                res.args.add loadType(ch)
                res.original.genericArgs.add GENERIC_LIST[i - 1]
          if res.original.label == "Block" or res.original.label == "Method":
            let args = res.args[0 .. ^2]
            let returnType = res.args[^1]
            res = Type(kind: T.Method, args: args, returnType: returnType)
          res
      else:
        nil
    else:
      nil
  echo result

proc loadSignature(child: PNode, returnType: PNode, typ: Type = nil): RewriteRule =
  result = RewriteRule()
  case child.kind:
  of nkObjConstr:
    if typ.isNil:
      result.input = Node(kind: Call, children: @[variable(child[0].ident.s)])
    else:
      result.input = Node(kind: Send, children: @[variable("self"), Node(kind: String, text: child[0].ident.s)])
    result.replacedPos = initTable[string, int]()
    var b = 0
    if not typ.isNil:
      result.args.add(@[])
      result.replaced.add((label: "self", typ: typ))
      result.replacedPos["self"] = 0
      result.replaced.add((label: "", typ: nil))
      b = 1
    for i, arg in child:
      if i > 0:
        let typ = loadType(arg[1])
        result.input.children.add(variable(arg[0].ident.s, typ=typ))
        result.args.add(@[])
        result.replaced.add((label: arg[0].ident.s, typ: typ))
        result.replacedPos[arg[0].ident.s] = i + b

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
  case ch.kind:
  of nkCall,:
    result.output = Node(kind: Call)
    var b = 0
    if ch[0].kind == nkDotExpr:
      if ch[0][0].kind == nkIdent:
        result.output = Node(kind: Send, children: @[variable(ch[0][0].ident.s), Node(kind: String, text: ch[0][1].ident.s)])
        # echo "fa ", "pos ", result.replacedPos
        if result.replacedPos.hasKey("self"):
          result.output.children[0] = nil
          result.replaceList.add((result.replacedPos["self"], @[0]))
          # echo "fa ", "self ", result.replaceList
        b = 1
      else:
        discard
    else:
      result.output.children = @[variable(ch[0].ident.s)]
    for i, arg in ch:
      # echo "fa ", i, " ", arg.e
      if i > 0:
        case arg.kind:
        of nkIdent, nkPrefix:

          var label = ""
          var child: Node
          if arg.kind == nkIdent:
            label = arg.ident.s
            child = nil
          else:
            assert arg[0].ident.s == "~"
            label = arg[1].ident.s
            child = variable(label)
            child.rewriteIt = true

          if result.replacedPos.hasKey(label):
            result.output.children.add(child)
            result.replaceList.add((result.replacedPos[label], @[i + b]))
            # echo "fa ", label, " ", result.replaceList
          else:
            result.output.children.add(variable(label))
        of nkCharLit..nkUInt64Lit:
          result.output.children.add(Node(kind: Int, i: arg.intVal.int))
        of nkFloatLit..nkFloat128Lit:
          result.output.children.add(Node(kind: Float, f: arg.floatVal.float))
        of nkStrLit..nkTripleStrLit:
          result.output.children.add(Node(kind: String, text: arg.strVal))
        of nkSym:
          discard
        else:
          discard
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
      if child[0].kind == nkObjConstr and child[0][0].kind == nkIdent:
        if child[0][0].ident.s == "typ":
          let typ = loadType(child[0][1][1])
          var dep: seq[string]
          echo child[0][1][1].e
          for typChild in child[1]:
            loadMapping(typChild, typ, dep, res)

        elif child[0][0].ident.s == "rewrite":
          var args: Table[string, Type]
          for arg in child[0][1]:
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
