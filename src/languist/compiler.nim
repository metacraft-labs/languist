import types, strformat, strutils, sequtils, ast_dsl, tables, json, gen_kind, sets, json, macros, terminal, helpers, os, osproc, sugar

# Praise the Lord!

proc underscore(label: string): string

proc rewriteType*(traceDB: TraceDB, label: string, typ: Type): Type =
  # rewrite a type from ruby based on module
  
  result = typ
  eecho &"TYP {label}"
  if traceDB.rewrite.types.len != 0:
    # first we check exact label and then the closest submatches 
    # e.g. A::B::C , A::B::*, A::*
    if traceDB.rewrite.types.hasKey(label):
      return traceDB.rewrite.types[label]
    let tokens = label.split("::")

    for i in countdown(tokens.len - 2, 0):
      let subLabel = tokens[0 .. i].join("::") & "::*"
      eecho &"FIND {subLabel}"
      if traceDB.rewrite.types.hasKey(subLabel):
        return traceDB.rewrite.types[subLabel]

proc loadType*(typ: JsonNode, traceDB: TraceDB): Type =
  # load a type from trace

  if typ{"kind"}.isNil:
    return nil
  
  var kind = parseEnum[T](typ{"kind"}.getStr())
  result = genKind(Type, kind)
  
  case kind:
  of T.Method:
    result.args = @[]
    # result.variables = @[]
    for arg in typ{"args"}:
      var variable = loadType(arg, traceDB)
      result.args.add(variable)
    if typ{"returnType"}.isNil:
      result.returnType = VoidType
    else:
      result.returnType = loadType(typ{"returnType"}, traceDB)
  of T.Object:
    result.fields = initTable[string, Type]()
    if typ{"base"} == nil:
      result.base = nil
    else:
      result.base = loadType(typ{"base"}, traceDB)
    for element in typ{"fields"}:
      let label = element{"fieldLabel"}.getStr()
      let typ = loadType(element, traceDB)
      result.fields[label] = typ
    result.isVar = false
    result.isRef = true
  of T.Simple:
    result.label = typ{"label"}.getStr()
  of T.Compound:
    result.args = @[]
    for arg in typ{"args"}:
      var variable = loadType(arg, traceDB)
      result.args.add(variable)
    result.original = loadType(typ{"original"}, traceDB)
  of T.Generic:
    result.label = typ{"label"}.getStr()
    result.genericArgs = @[]
    for arg in typ{"genericArgs"}:
      result.genericArgs.add(arg.getStr())
  else:
    discard
  if result.kind != T.Simple:
    if typ{"label"}.isNil:
      result.label = ""
    else:
      result.label = typ{"label"}.getStr()
  if result.kind in {T.Simple, T.Object}:
    result = rewriteType(traceDB, result.label, result)
    if "::" in result.label:
      # TODO module
      result.label = result.label.rsplit("::", 1)[1]


proc loadMethod*(m: JsonNode, traceDB: TraceDB, isBlock: bool = false): Node

proc loadNode*(m: JsonNode, traceDB: TraceDB): Node =
  # load a node from trace

  if m{"kind"}.isNil:
    return nil
  var kind = parseEnum[NodeKind](m{"kind"}.getStr())
  case kind:
  of Variable:
    result = Node(kind: Variable, label: m{"label"}.getStr())
  of Operator:
    result = Node(kind: Operator, label: m{"label"}.getStr())
  of RubyConst:
    result = Node(kind: RubyConst, label: m{"label"}.getStr())
  of Int:
    result = Node(kind: Int, i: m{"i"}.getInt(), typ: IntType)
  of Bool:
    result = Node(kind: Bool, val: m{"val"}.getBool(), typ: BoolType)
  of String:
    result = Node(kind: String, text: m{"text"}.getStr(), typ: StringType)
  
  of Docstring:
    result = Node(kind: Docstring, text: m{"text"}.getStr(), typ: StringType)
  of Symbol:
    result = Node(kind: Symbol, text: m{"text"}.getStr(), typ: SymbolType)
  of Block:
    # FAITH
    return loadMethod(m, traceDB, isBlock=true)
  else:
    result = genKind(Node, kind)
    if m{"children"}.isNil:
      discard
    else:
      result.children = m{"children"}.mapIt(loadNode(it, traceDB))
      if not m{"label"}.isNil:
        result.label = m{"label"}.getStr()
    if kind == Assign:
      if not m{"declaration"}.isNil:
        result.declaration = parseEnum[Declaration](m{"declaration"}.getStr())
  # Faith
  result.typ = loadType(m{"typ"}, traceDB)

proc loadMethod*(m: JsonNode, traceDB: TraceDB, isBlock: bool = false): Node =
  result = Node(kind: NodeMethod)
  if isBlock:
    result = Node(kind: Block)
  if traceDB.lang == Lang.Ruby:
    result.label = m{"label"}{"label"}.getStr()
  else:
    result.label = m{"label"}.getStr()
  result.args = m{"args"}.mapIt(loadNode(it, traceDB))
  result.code = m{"code"}.mapIt(loadNode(it, traceDB))
  result.isIterator = m{"isIterator"}.getBool()
  result.typ = loadType(m{"typ"}, traceDB)
  result.returnType = loadType(m{"returnType"}, traceDB)
  if not m{"docstring"}.isNil:
    result.docstring = m{"docstring"}.mapIt(it.getStr())
  else:
    result.docstring = @[]
  var annotations: seq[Annotation]
  
  for docstring in result.docstring:
    if docstring.startsWith("l "):
      annotations.add(parseAnnotation(docstring.strip))
  result.annotations = annotations
  
  if annotations.len > 0 and annotations[0].kind == AnnotationKind.Ignore:
    return nil

  if result.typ.isNil or traceDB.lang == Lang.Python:
    var args = result.args.mapIt(it.typ)
    result.typ = Type(kind: T.Method, args: args, returnType: result.returnType)
  # echo result.label
  # echo result.typ
  if traceDB.lang == Lang.Python:
    result.typ.returnType = result.returnType
  traceDB.methods[result.label] = result.typ

proc loadClass*(m: JsonNode, traceDB: TraceDB): Node =
  result = Node(kind: Class)
  result.label = m{"label"}.getStr()
  result.fields = m{"fields"}.mapIt(Field(label: it{"label"}.getStr(), node: it{"node"}.loadNode(traceDB)))
  result.methods = m{"methods"}.mapIt(Field(label: it{"label"}.getStr(), node: it{"node"}.loadMethod(traceDB))).filterIt(not it.node.isNil)
  result.docstring = m{"docstring"}.mapIt(it.getStr())
  var typ = loadType(m{"typ"}, traceDB)
  edump result.label
  edump dump(typ, 0)
  if typ.isNil or typ.kind notin {Simple, Object}:
    # TODO
    result.typ = Type(kind: T.Object, fields: initTable[string, Type]())
  elif traceDB.types.hasKey(typ.label):
    result.typ = traceDB.types[typ.label]
  else:
    result.typ = typ
  if result.typ.rewritten:
    return nil

proc loadModule*(m: JsonNode, traceDB: TraceDB, path: string): Module =
  result = Module()
  result.imports = @[]
  result.path = path
  result.main = m{"main"}.mapIt(loadNode(it, traceDB))
  result.classes = m{"classes"}.mapIt(loadClass(it, traceDB)).filterIt(not it.isNil)

var rewriteLangs*: array[Lang, Rewrite]

proc load*(file: string, targetFolder: string, config: Config, lang: Lang): TraceDB =
  new(result)
  result.root = parseJson(readFile(file))
  result.types = initTable[string, Type]()
  result.sysPath = @[]
  result.paths = @[]
  result.targetFolder = targetFolder
  result.modules = @[]
  result.rewrite = rewriteLangs[lang]
  result.config = config
  result.methods = initTable[string, Type]()
  result.lang = lang
  # result.projectDir = result.root{"@projectDir"}.getStr()
  # result.package = result.projectDir.rsplit("/", 1)[1]
  for label, m in result.root:
    for typ, obj in m:
      result.types[typ] = loadType(obj, result)
      if "::" in typ:
        # TODO
        # cant think of the smarter way
        result.types[typ.rsplit("::", 1)[1]] = result.types[typ]
  for label, m in result.root:
    if label != "%types":
      if label != "tracing.rb":
        result.paths.add(label)
        result.modules.add(loadModule(m, result, label))
    

  
proc startPath*(db: TraceDB): string =
  # TODO: smarter
  result = ""
  var maybeResult = ""
  for module in db.paths:
    if module.startsWith(db.projectDir):
      if module.endsWith("constants.py"):
        result = module
        break
      elif maybeResult == "":
        maybeResult = module
  if result == "":
    result = maybeResult


proc accepts(l: Type, r: Type): bool =
  if l.isNil:
    return false
  if l.kind == T.Any:
    return true
  elif r.isNil:
    return false
  elif l.kind == T.Method and l.kind == r.kind and l.returnType.isNil:
    return true
  elif l.kind == T.Generic and r.kind == T.Compound and l.label == r.original.label:
    return true
  else:
    return l == r    

proc normalize(label: string): string =
  result = camelCase(label)  

func compatible(l: string, r: string): bool =
  if '?' in r:
    var good = "is_" & r[0 .. ^2]
    good = good.normalize
    var goodL = l.normalize
    goodL == good
  else:
    l.normalize == r.normalize
  

var t = false
proc find(l: Node, r: Node, replaced: seq[tuple[label: string, typ: Type]]): bool =
  # if l.kind == Index and r.kind == Index:
  #   t = true
  if t:
    echo l
    echo r
  if l.kind == Variable:
    var replace = false
    var typ: Type = nil
    for member in replaced:
      if member.label == l.label:
        replace = true
        typ = member.typ
        break
    if replace:
      if not typ.isNil and not typ.accepts(r.typ):
        return false
      else:
        return true
    else:
      if l.label == "self" and r.kind == Self:
        return true
      return compatible(l.label, r.label)

  if l.kind == Operator and r.kind in {Variable, Operator}:
    return compatible(l.label, r.label)

  if l.kind == String and r.kind in {String, Symbol}:
    return compatible(l.text, r.text)
  if l.kind != r.kind:
    return false
  case l.kind:
  of Variable, Operator, RubyConst:
    result = compatible(l.label, r.label)
  of Docstring:
    result = compatible(l.text, r.text)
  of Symbol:
    result = compatible(l.text, r.text)
  of Int:
    result = l.i == r.i
  of Bool:
    result = l.val == r.val
  of Float:
    result = l.f == r.f
  of Char:
    result = l.c == r.c
  else:
    result = true
  if not result:
    if l.kind == Index and r.kind == Index:
      echo "not result"
    return
  if l.children.len != r.children.len:
    if l.kind == Index and r.kind == Index:
      t = false
      echo "no"
    return false
  for i in 0 .. < l.children.len:
    edump i
    if not l.children[i].find(r.children[i], replaced):
      edump l.children[i]
      if l.kind == Index and r.kind == Index:
        echo "not equal"
        t = false
      return false
  if l.kind == Index and r.kind == Index:
    echo "true"
    t = false
  result = true
  
proc find(rewrite: Rewrite, node: Node): seq[RewriteRule] =
  result = @[]
  for rule in rewrite.rules:
    if rule.input.find(node, rule.replaced):
      result.add(rule)

var rewriteList = Rewrite(rules: @[], types: initTable[string, Type](), genBlock: @[], symbolRules: @[], lastCalls: @[])
var rewrites: seq[Rewrite] = @[]

proc replace(rule: RewriteRule, node: Node, blockNode: Node, m: Module): Node =
  var args = initTable[string, Node]()
  # dump node
  for i in 0 ..< rule.replaced.len:
    let r = rule.replaced[i].label
    let arg = rule.args[i]
    var k = 0
    var subNode = node
    while k < arg.len:
      subNode = subNode.children[rule.args[i][k]]
      k += 1
    args[r] = subNode
  result = rule.output(node, args, blockNode, rule)
  for dependency in rule.dependencies:
    var existing = false
    for im in m.imports:
      if im.kind == Import and im.children[0].kind == Variable and im.children[0].label == dependency:
        existing = true
        break
    if not existing:
      m.imports.add(Node(kind: Import, children: @[variable(dependency)]))
  result.isFinished = true

proc rewriteChildren(node: Node, rewrite: Rewrite, blockNode: Node, m: Module): Node


proc rewriteNode(node: Node, rewrite: Rewrite, blockNode: Node, m: Module): Node =
  var b: seq[RewriteRule] = @[]
  if not node.isFinished:
    b = rewrite.find(node)
  var newNode = node
  if node.kind == Call and node.children[0].kind == Variable and node.children[0].label.underscore in rewrites[2].genBlock:
    node.kind = MacroCall # = genKind(Node, MacroCall)
    if node.children[^1].kind == Block:
      node.children[^1] = Node(kind: Code, children: node.children[^1].code)
  elif node.kind == Send and node.children[0].kind == Self and node.children[1].text.underscore in rewrites[2].genBlock:
    let boolean = node.children[1].text.endsWith("?")
    if not boolean:
      node.kind = MacroCall
    else:
      node.kind = Call
    node.children = @[Node(kind: Variable, label: node.children[1].text)].concat(node.children[2 .. ^1])
    if not boolean:
      node.children[^1] = Node(kind: Code, children: node.children[^1].code)

  if b.len > 0:
    var c = b[0]
    for a in b:
      if c.isGeneric and not a.isGeneric:
        c = a
    newNode = c.replace(node, blockNode, m)
  var last = false
  if node.kind == Symbol:
    for rule in rewrite.symbolRules:
      if rule.label == "_" and node.text in rule.elements:
        newNode = rule.handler(node.text)
        echo newNode
    for rule in rewrite.lastCalls:
      if node.text in rule.elements:
        newNode = rule.handler(node.text)
        echo newNode
  if node.kind == Call and node[0].kind == Variable:
    for rule in rewrite.symbolRules:
      if compatible(node[0].label, rule.label):
        rewrite.lastCalls.add(rule)
        last = true
        break
  elif node.kind == Send:
    for rule in rewrite.symbolRules:
      if compatible(node[0].label, rule.label):
        rewrite.lastCalls.add(rule)
        last = true
        break

  result = rewriteChildren(newNode, rewrite, blockNode, m)
  if last:
    discard rewrite.lastCalls.pop


proc rewriteChildren(node: Node, rewrite: Rewrite, blockNode: Node, m: Module): Node =
  var newNode = deepCopy(node)
  if node.kind == Class:
    for i, met in node.methods:
      newNode.methods[i].node = rewriteNode(met.node, rewrite, blockNode, m)

  elif node.kind == NodeMethod or node.kind == Block:
    for i, child in node.code:
      if not child.isNil:
        newNode.code[i] = rewriteNode(child, rewrite, blockNode, m)
      else:
        newNode.code[i] = nil
    
    for i, child in node.children:
      if not child.isNil:
        newNode.children[i] = rewriteNode(child, rewrite, blockNode, m)
      else:
        newNode.children[i] = nil
  else:
    for i, child in node.children:
      if not child.isNil:
        newNode.children[i] = rewriteNode(child, rewrite, blockNode, m)
      else:
        newNode.children[i] = nil
  return newNode

var inRuby {.compileTime.} = true

proc compileNode(node: NimNode, replaced: seq[(string, NimNode)], isOutputArg: bool = false): NimNode =
  var typ: NimNode
  var isOutput = isOutputArg
  if node.kind == nnkDo and isOutput:
    typ = ident($node[3][0] & "Type")
    result = compileNode(node[^1], replaced, isOutput)
    if result[0].repr in @["call"]:
      result.del(result.len - 1)
    result.add(typ)
    return
  # dump node.lisprepr
  case node.kind
  of nnkCharLit..nnkUInt64Lit:
    return node
  of nnkFloatLit..nnkFloat128Lit:
    return node
  of nnkStrLit..nnkTripleStrLit:
    return node
  of nnkSym:
    return nnkCall.newTree(ident"variable", newLit($node))
  of nnkIdent:
    let label = $node
    var returnType = newNilLit()
    if not isOutput:
      for e in replaced:
        if label == e[0]:
          typ = e[1]
      return nnkCall.newTree(ident"variable", newLit($node), returnType)
    else:
      #dump replaced
      for e in replaced:
        #dump e
        if label == e[0]:
          return nnkBracketExpr.newTree(ident"args", newLit($node))
      return nnkCall.newTree(ident"variable", newLit($node), returnType)
  of nnkAccQuoted:
    # Praise the Lord!
    return compileNode(node[0], replaced, isOutput)
  of nnkStmtList:
    return compileNode(node[0], replaced, isOutput)
  else:
    var sons = node.mapIt(compileNode(it, replaced, isOutput))
    var call = ""
    case node.kind:
    of nnkCall, nnkCommand:
      call = "call"
      if node[0].kind == nnkDotExpr:
        call = "send"
        var oldSons = sons
        sons = @[]
        sons.add(oldSons[0][1])
        sons.add(oldSons[0][2])
        sons = sons.concat(oldSons[1 .. ^1])
        if inRuby and ($sons[1]).startsWith("is_"):
          sons[1] = newLit(($(sons[1]))[3 .. ^1] & "?")
      else:
        let voidNode = ident("VoidType")
        sons.add(voidNode)
    # Praise the Lord!
    of nnkPrefix:
      call = "call"
    of nnkDotExpr:
      call = "attribute"
      sons[1] = sons[1][1]
    of nnkInfix:
      call = "binop"
      sons[0][0] = ident("operator")
    of nnkBracketExpr:
      call = "index"
      if sons[1].kind == nnkStrLit:
        let textNode = sons[1]
        sons[1] = quote do: Node(kind: String, text: `textNode`)
    else:
      call = "unknown"
    result = nnkCall.newTree(ident(call))
    for son in sons:
      result.add(son)
    return result


proc args(node: NimNode, arg: string, a: var seq[int]): bool =
  case node.kind:
  of nnkCall:
    if node[0].kind == nnkIdent and $node[0] == "variable" and $node[1] == arg:
      return true
    else:
      for i, child in node:
        let iArg = i - 1
        if iArg == -1:
          continue
        
        a.add(i - 1)
        let res = args(child, arg, a)
        if res:
          return true
        else:
          discard a.pop()
      return false
  else:
    return false


proc args(node: NimNode, replaced: seq[(string, NimNode)]): seq[seq[int]] =
  result = @[]
  for arg in replaced:
    result.add(@[])
    discard args(node, arg[0], result[^1])
    
proc generateInput(input: NimNode): (NimNode, seq[(string, NimNode)]) =
  let args = input[3]
  var help = ident("help")
  var res = quote:
    var `help` = RewriteRule(input: nil, output: nil, replaced: @[], isGeneric: false)
  res = nnkStmtList.newTree(res)
  var replaced2: seq[(string, NimNode)]

  for i, arg in args:
    if i != 0:
      let label = newLit($arg[0])
      let typ = ident($arg[1] & "Type")
      var n = quote:
        `help`.replaced.add((
          `label`,
          `typ`))
      res.add(n)
      replaced2.add(($label, typ))

  var h = compileNode(input[^1], replaced2)
  if h.kind == nnkStrLit:
    h = quote do: Node(kind: String, text: `h`)
  var n = quote:
    `help`.input = `h`
  res.add(n)
  let args2 = args(h, replaced2)
  n = quote:
    @[]
  for argList in args2:
    var m = quote:
      @[]
    for arg in argList:
      m[1].add(newLit(arg))
    n[1].add(m)
  n = quote:
    `help`.args = `n`
  res.add(n)
  n = quote:
    rewriteList.rules.add(`help`)
  res.add(n)
  result = (res, replaced2)

proc rewriteLabel(code: Node, label: string, newLabel: string) =
  if code.kind == Variable:
    if code.label == label:
      code.label = newLabel
  else:
    for child in code.children:
      rewriteLabel(child, label, newLabel)

proc rewriteIt(code: Node): Node =
  var element = code.args[0].label
  for child in code.code:
    rewriteLabel(child, element, "it")
  result = Node(kind: Code, children: code.code)

macro rewrite*(input: untyped, output: untyped): untyped =
  if input.kind == nnkStrLit:

    let node = output
    result = quote:
      rewriteList.types[`input`] = `node`
      rewriteList.types[`input`].rewritten = true
    return result
  let (inputNode, replaced) = generateInput(input)
  result = inputNode
  let help = ident("help")
  var outputNode: NimNode
  dump output.lisprepr
  var dependencies: NimNode = quote:
    @[]
  if output.kind == nnkStmtList and output[0][0].repr == "code":
    outputNode = output[0][1]
    if output.len > 1:
      dependencies = output[1][1]
  else:
    outputNode = compileNode(output, replaced, true)
  let code = outputNode
  let outputCall = nnkLambda.newTree(
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      newIdentNode("Node"),
      nnkIdentDefs.newTree(
        newIdentNode("node"),
        newIdentNode("Node"),
        newEmptyNode()
      ),
      nnkIdentDefs.newTree(
        newIdentNode("args"),
        nnkBracketExpr.newTree(
          newIdentNode("Table"),
          newIdentNode("string"),
          newIdentNode("Node")
        ),
        newEmptyNode()
      ),
      nnkIdentDefs.newTree(
        newIdentNode("blockNode"),
        newIdentNode("Node"),
        newEmptyNode()
      ),
      nnkIdentDefs.newTree(
        newIdentNode("rule"),
        newIdentNode("RewriteRule"),
        newEmptyNode()
      )
    ),
    newEmptyNode(),
    newEmptyNode(),
    code
  )


  var n = quote:
    `help`.output = `outputCall`
  result.add(n)
  n = quote:
    `help`.dependencies = `dependencies`
  result.add(n)
  result = quote:
    block:
      `result`
  dump result.repr

macro symbols*(input: untyped): untyped =
  result = nnkStmtList.newTree()
  for child in input:
    let callLabel = newLit(child[0].repr)
    let c = child[1]
    let callCode = child[2]
    var aNode = ident("a")
    let symbol = quote:
      var l = proc(a: string): Node =
        var `aNode` = a
        `callCode`
      rewriteList.symbolRules.add(SymbolRule(label: `callLabel`, elements: `c`, handler: l))
    result.add(symbol)
  echo result.repr

proc params*(p: Table[string, string]) =
  rewriteList.params = p

include rewrites/ruby_rewrite

include rewrites/ruby_plugin

var rewriteRuby* = rewriteList
rewriteList = Rewrite(rules: @[], types: initTable[string, Type](), genBlock: @[], symbolRules: @[], lastCalls: @[])
rewriteLangs[Lang.Ruby] = rewriteRuby

include rewrites/nim_rewrite

var rewriteNim = rewriteList
rewriteList = Rewrite(rules: @[], types: initTable[string, Type](), genBlock: @[], symbolRules: @[], lastCalls: @[])

include rewrites/python_rewrite

var rewritePython* = rewriteList
rewrites = @[rewriteRuby, rewritePython, rewriteNim]
rewriteLangs[Lang.Python] = rewritePython

var A = Type(kind: T.Object, label: "A", fields: initTable[string, Type]())

proc toBool(node: Node): Node =
  if node.typ == IntType:
    let child = node
    result = compare(operator("!="), child, 0, BoolType)
  elif node.typ == StringType:
    let child = node.children[0]
    result = call(variable("not_empty"), child, BoolType)
  else:
    result = node

proc underscore(label: string): string =
  result = ""
  for i, c in label:
    if c.isUpperAscii:
      if i > 0:
        result.add("_")
      result.add(($c).toLowerAscii)
    else:
      result.add($c)
  if result.endsWith("?"):
    result = "is_" & result[0 .. ^2]

proc analyze(node: Node, env: Env, class: Type = nil, inBranch: bool = false) =
  case node.kind:
  of Class:
    for met in node.methods.mitems:
      analyze(met.node, env, node.typ)
  of NodeMethod, Block:
    var args = initTable[string, Type]()
    for arg in node.args:
      args[arg.label] = arg.typ
    if node.kind == NodeMethod:
      args["self"] = class
    if node.label == "to_s":
      node.returnType = StringType
      node.typ.returnType = StringType
      node.label = "$"
    elif node.label == "to_i":
      node.returnType = IntType
      node.typ.returnType = IntType
      node.label = "int"
    elif node.label == "initialize":
      node.label = "init" & class.label
      node.returnType = class
      node.typ.returnType = class
      node.args = node.args[1 .. ^1]
      node.code = @[call(variable("new"), variable("result"), VoidType)].concat(node.code)
    # TODO
    if node.args.len > 0 and node.args[0].label == "self":
      node.isMethod = true

    var met = childEnv(env, node.label, args, node.returnType)

    for subNode in node.code:
      analyze(subNode, met)
    for annotation in node.annotations:
      if annotation.kind == AnnotationKind.NameType:
        met[annotation.name] = annotation.typ
    var declarations: seq[Node] = @[]
    for label, t in met.declarations:
      let typ = t[0]
      let declaration = t[1]
      declarations.add(assign(variable(label), nil, declaration))
      declarations[^1].typ = typ
    # FAITH
    node.code = declarations.concat(node.code)
    if node.code[^1].typ.isNil:
      node.returnType = VoidType

  of Send, Call:
    analyze(node.children[0], env, inBranch=inBranch)
    var children = node.children[1 .. ^1]
    var newChildren: seq[Node]
    if node.kind == Send:
      if node.children[1].text.endsWith("?"):
        node.children[1].text = "is_" & node.children[1].text[0 .. ^2]
      children = node.children[2 .. ^1]
      newChildren.add(node.children[1])
    for arg in children:
      if arg.kind != RubySplat:
        analyze(arg, env, inBranch=inBranch)
        newChildren.add(arg)
      else:
        assert arg.children.len == 1
        let label = if node.kind == Call: node.children[0].label else: node.children[1].text
        dump label
        if env.hasKey(label):
          let typ = env[label]
          if typ.kind == Method:
            let length = if node.kind == Call: typ.args.len else: typ.args.len - 1
            for i in 0 ..< length:
              let childType = if not arg.children[0].typ.isNil and arg.children[0].typ.kind == Compound and arg.children[0].typ.args.len > 0: arg.children[0].typ.args[0] else: arg.children[0].typ
              newChildren.add(index(arg.children[0], Node(kind: Int, i: i), childType))

    node.children = @[node.children[0]].concat(newChildren)
    if node.kind == Call and node.children[0].kind == Variable and node.children[0].label == "include":
      node.children[1].label = underscore(node.children[1].label)
  of Variable:
    if node.label.endsWith("?") and node.label.len > 1:
      eecho node.label
      node.label = "is_" & node.label[0 .. ^2]
    node.typ = env.get(node.label)
  of RubyConst:
    node.typ = Type(kind: T.Simple, label: "Class")
  of Int:
    node.typ = IntType
  of Bool:
    node.typ = BoolType
  of Assign:
    analyze(node.children[1], env, inBranch=inBranch)
    node.children[0].typ = node.children[1].typ
    if node.children[0].kind == Variable:
      if not env.types.hasKey(node.children[0].label) and node.declaration == Existing:
        if not inBranch:
          node.declaration = Var
        else:
          node.declaration = Existing
          if not env.declarations.hasKey(node.children[0].label):
            env.declarations[node.children[0].label] = (node.children[0].typ, Var)
        env[node.children[0].label] = node.children[0].typ
  of String, Docstring:
    node.typ = StringType
  of Symbol:
    node.typ = SymbolType
  of New:
    node.typ = Type(kind: T.Simple, label: node.children[0].label)
  of Sequence:
    for child in node.children:
      analyze(child, env, inBranch=inBranch)
    if node.children.len > 0:
      node.typ = sequenceType(node.children[0].typ)
  of NimTable:
    for child in node.children:
      analyze(child[0], env, inBranch=inBranch)
      analyze(child[1], env, inBranch=inBranch)
    if node.children.len > 0:
      node.typ = tableType(node.children[0][0].typ, node.children[0][1].typ)
  of While:
    analyze(node.children[0], env, inBranch=true)
    node.children[0] = toBool(node.children[0])
    for child in node.children[1 .. ^1]:
      analyze(child, env, inBranch=true)
  of Attribute:
    analyze(node.children[0], env, inBranch=inBranch)
    if not node.children[0].typ.isNil and node.children[0].typ.kind == Object:
      if node.children[0].typ.fields.hasKey(node.children[1].text):
        let typ = node.children[0].typ.fields[node.children[1].text]
        node.typ = typ
      else:
        node.typ = VoidType
    else:
      node.typ = VoidType
  of Self:
    node.typ = if env.hasKey("self"): env["self"] else: VoidType
  of BinOp:
    for child in node.children[1 .. ^1]:
      analyze(child, env, inBranch=inBranch)
    if node.children[0].label in @["and", "or", ">", "<", "==", "!=", "&&", "||", ">=", "<="]:
      node.typ = BoolType
    if node.typ == BoolType and node.children[0].label == "and":
      node.children[1] = toBool(node.children[1])
      node.children[2] = toBool(node.children[2])
    elif node.children[0].label == "+":
      node.typ = node.children[1].typ
  of UnaryOp:
    if node.children[0].label in @["not", "!"]:
      node.typ = BoolType
      node.children[0].label = "not"
    # echo node.children[0]
  of If:
    analyze(node.children[0], env, inBranch=true)
    node.children[0] = toBool(node.children[0])
    for child in node.children[1 .. ^1]:
      analyze(child, env, inBranch=true)
  else:
    for child in node.children:
      analyze(child, env, inBranch=inBranch)

proc analyze(m: Module, env: Env) =
  for child in m.classes:
    analyze(child, env)
  for child in m.main:
    analyze(child, env)

proc analyzeCode(traceDB: TraceDB, env: Env) =
  for input in traceDB.modules:
    edump dump(input, 0, true)
    for child in input.classes:
      env[child.label] = child.typ
  for input in traceDB.modules:
    input.analyze(env)






proc rewriteProgram(node: Node, rewrite: Rewrite, m: Module): Node =
  case node.kind:
  of Class:
    result = node.deepCopy()
    for i, met in node.methods:
      var newCode: seq[Node] = @[]
      for element in met.node.code:
        newCode.add(rewriteNode(element, rewrite, met.node, m))
      result.methods[i].node.code = newCode
  else:
    discard

proc rewriteProgram(m: Module, rewrite: Rewrite): Module =
  result = m
  result.classes = m.classes.mapIt(rewriteProgram(it, rewrite, m))
  result.main = m.main.mapIt(rewriteNode(it, rewrite, nil, m))

proc rewriteCode(traceDB: TraceDB, rewrite: Rewrite) =
  for i, input in traceDB.modules:
    var newInput = rewriteProgram(input, rewrite)
    traceDB.modules[i] = newInput

include generator

proc generateCode(traceDB: TraceDB) =
  for i, input in traceDB.modules:
    let path = traceDB.paths[i]
    let nimPath = path.changeFileExt("nim")
    let folder = nimPath.parentDir.splitFile[1]
    let base = nimPath.splitFile[1]
    let newPath = traceDB.targetFolder / folder / base & ".nim"
    var generator = Generator(indent: 2, v: V019, module: Module(), identifierCollisions: initSet[string](), lang: traceDB.lang, params: rewrites[0].params)
    var output = generator.generate(input, traceDB.config)
    createDir traceDB.targetFolder / folder
    writeFile(newPath, output)
    echo &"write {newPath}"


proc compile*(traceDB: TraceDB) =
  var env = Env(parent: nil, types: initTable[string, Type]())
  for label, t in traceDB.methods:
    env[label] = t
  # assign type names
  analyzeCode(traceDB, env)

  # rewrite ruby code
  rewriteCode(traceDB, traceDB.rewrite)
  # rewrite nim code
  rewriteCode(traceDB, rewriteNim)

  traceDB.generateCode 
