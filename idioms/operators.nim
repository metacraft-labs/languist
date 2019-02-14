import tables, strutils, sequtils, strformat
import idioms_dsl, ../python_types, ../nim_types, ../python_ast, ../ast_dsl, ../core

operators:
  # int https://docs.python.org/2/library/stdtypes.html#numeric-types-int-float-long-complex
  `+`(T.Int, T.Int) -> T.Int

  `-`(T.Int, T.Int) -> T.Int

  `*`(T.Int, T.Int) -> T.Int

  `/`(T.Int, T.Int) -> T.Int

  `//`(T.Int, T.Int):
    binop(left, operator("div"), right, typ=T.Int)

  `**`(T.Int, T.Int):
    binop(left, operator("^"), right, typ=T.Int)

  `<<`(T.Int, T.Int):
    binop(left, operator("shl"), right, typ=T.Int)

  `>>`(T.Int, T.Int):
    binop(left, operator("shr"), right, typ=T.Int)

  `&`(T.Int, T.Int):
    binop(left, operator("and"), right, typ=T.Int)

  `|`(T.Int, T.Int):
    binop(left, operator("or"), right, typ=T.Int)

  # float https://docs.python.org/2/library/stdtypes.html#numeric-types-int-float-long-complex
  `+`(T.Float, T.Float) -> T.Float

  `-`(T.Float, T.Float) -> T.Float

  `*`(T.Float, T.Float) -> T.Float

  `/`(T.Float, T.Float) -> T.Float

  `**`(T.Float, T.Float):
    call(label("pow"), @[left, right], T.Float)

  `+`(T.String, T.String):
    binop(left, operator("&"), right, typ=T.String)

  `+`(T.List, T.List):
    call(attribute(left, "concat"), @[right], typ=left.typ)

# echo operatorTypes
