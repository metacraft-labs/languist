rewriteGenerator["lib/rubocop/cop"] = proc(generator: Generator): PNode =
  nkStmtList.newTree(
    generator.top,
    nkCommand.newTree(
      generateIdent("cop"),
      nkStmtList.newTree(
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