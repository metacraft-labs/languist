rewriteGenerator["lib/rubocop/cop"] = proc(generator: Generator): PNode =
  let t = generator.types[0][0][0][1] 
  var docstring: PNode
  docstring = generator.types[0][^1]
  var newTypes = nkStmtList.newTree()
  for i, child in generator.types:
    if i > 0:
      newTypes.add(child)

  generator.types = newTypes
  nkStmtList.newTree(
    generator.top,
    nkCommand.newTree(
      generateIdent("cop"),
      t,
      nkStmtList.newTree(
        docstring,
        generator.types,
        generator.global,
        generator.methods,
        generator.main)))

rewriteGenerator["spec/rubocop/cop"] = proc(generator: Generator): PNode =
  let name = generator.module.path.splitFile[1].split("_spec")[0]
  generator.top.add(nkImportStmt.newTree(
    generateDirectIdent(name),
    generateDirectIdent("test_tools")))
  nkStmtList.newTree(
    generator.top,
    generator.types,
    generator.global,
    generator.methods,
    generator.main)