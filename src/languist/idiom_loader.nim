# Praise the Lord!

import types, 
  compiler / [
    parser, idents, msgs, configuration, 
    ast, options,
    condsyms,nimconf, extccomp, pathutils],
  sequtils, strutils, strformat, tables, os, ast_dsl

proc loadSignature(child: PNode, typ: Type = nil): RewriteRule =
  result = RewriteRule(input: nil)

proc loadCode(child: PNode, signature: RewriteRule): RewriteRule =
  # just a node and instructions what to do
  # like 0: y.mainArg and way to fix it: create a node with those nil nodes! but replace
  var output: Node
  case child.kind:
  of nkCall:
    output = Node(kind: Call)
    output.children = @[variable($child[0])]
    for i, arg in child:
      if i > 0:
       otuput.children.add(nil)
  else:
    discard
  result = RewriteRule(input: signature.input, output: output)

proc loadMapping(child: PNode, typ: Type, dep: var seq[string], res: var Rewrite) =
  case child.kind:
  of nkCommand:
    # dep name
    if $child[0] == "dep":
      dep = @[$(child[1])]
  of nkCall:
    # signature: right
    var signature = loadSignature(child[0], typ)
    var right = loadCode(child[1], signature)
    right.dependencies = dep
    res.rules.add(right)
  else:
    discard
    
proc loadDSL*(dsl: PNode, res: var Rewrite) =
  for child in dsl:
    if child.kind == nkCall:
      case child[0].kind:
      of nkInfix:
        # signature: right
        var dep: seq[string]
        loadMapping(child, nil, dep, res)
      of nkCall:
        if $child[0][0] == "typ":
          # typ(T): children
          let typ = loadType(child[1])
          var dep: seq[string]
          for typChild in child[2]:
            loadMapping(typChild, typ, dep)

        elif $child[0][0] == "rewrite":
          # rewrite(a: A, b: B): children
          var args = initTable[string, Type]()
          for arg in child[1]:
            args[$arg[0]] = loadType(arg[1])
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
    echo "parse", package.id
    var node = parseString(source, cache, conf)
    loadDSL(node, result)

var rewrites*: seq[Rewrite] = @[]

proc loadIdioms*(traceDB: TraceDB) =
  for package in traceDB.config.idioms:
    var rewrite = loadRewrite(package)
    if not rewrite.isNil:
      rewrites.add(rewrite)
