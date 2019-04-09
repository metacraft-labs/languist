import macros

macro genKind*(v: typed, s: typed): untyped =
  var kind = newIdentNode(repr(s))
  result = nnkCaseStmt.newTree(
    s)
  var sType = s.getType()
  # echo treerepr(sType)
  var z = 0
  for child in sType:
    if z > 0:
      result.add(nnkOfBranch.newTree(
        newIdentNode(repr(child)),
        nnkStmtList.newTree(
          nnkObjConstr.newTree(
            v,
            nnkExprColonExpr.newTree(
              newIdentNode("kind"),
              newIdentNode(repr(child)))))))
    z += 1
  # echo repr(result)
