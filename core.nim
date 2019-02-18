import strutils, sequtils, tables, algorithm, strformat
import types

# var IntType*      = Type(kind: T.Atom, label: "int")
# var HugeIntType*  = Type(kind: T.Atom, label: "HugeInt")
# var FloatType*    = Type(kind: T.Atom, label: "float")
# var BoolType*     = Type(kind: N.Atom, label: "bool")
# var CharType*     = Type(kind: N.Atom, label: "char")
# var StringType*   = Type(kind: N.Atom, label: "string")
# var VoidType*     = Type(kind: N.Atom, label: "void")
# var SeqType*      = Type(kind: N.Generic, label: "seq", genericArgs: @["T"])
# var TableType*    = Type(kind: N.Generic, label: "Table", genericArgs: @["K", "V"])
# var CStringType*  = Type(kind: N.Atom, label: "cstring")

# var ListType* = SeqType
# var DictType* = TableType
# var BytesType* = CStringType

# let T* = Core(
#   Int: IntType,
#   HugeInt: HugeIntType,
#   Float: FloatType,
#   Bool: BoolType,
#   Char: CharType,
#   String: StringType,
#   Void: VoidType,
#   Seq: SeqType,
#   Table: TableType,
#   List: ListType,
#   Dict: DictType,
#   Bytes: BytesType
# )

# proc seqType*(element: Type): Type =
#   result = Type(kind: N.Compound, args: @[element], original: T.List)

# proc tableType*(key: Type, value: Type): Type =
#   result = Type(kind: N.Compound, args: @[key, value], original: T.Dict)

# proc pyLabel*(label: string, typ: Type = nil): Node =
#   result = Node(kind: PyLabel, label: label, typ: typ)

# proc pyInt*(i: int): Node =
#   result = Node(kind: PyInt, i: i, typ: T.Int)

# proc pyHugeInt*(h: string): Node =
#   result = Node(kind: PyHugeInt, h: h, typ: T.HugeInt)

# proc pyFloat*(f: float): Node =
#   result = Node(kind: PyFloat, f: f, typ: T.Float)

# proc pyBool*(b: bool): Node =
#   result = Node(kind: PyLabel, label: ($b).capitalizeAscii(), typ: T.Bool)

# proc pyString*(s: string, typ: Type = StringType): Node =
#   result = Node(kind: PyStr, s: s, typ: typ)

# proc pyChar*(c: char): Node =
#   result = Node(kind: PyChar, c: c, typ: T.Char)

# proc pySeq*(children: seq[Node], typ: Type = nil): Node =
#   result = Node(kind: PyList, children: children)
#   if typ == nil and len(children) > 0 and children[0].typ != nil:
#     result.typ = seqType(children[0].typ)
#   else:
#     result.typ = nil

# proc pyTable*(keys: seq[Node], values: seq[Node], typ: Type = nil): Node =
#   result = Node(kind: PyDict, children: @[Node(kind: Sequence, children: keys), Node(kind: Sequence, children: values)])
#   if typ == nil and len(keys) > 0 and len(values) > 0 and keys[0].typ != nil and values[0].typ != nil:
#     result.typ = tableType(keys[0].typ, values[0].typ)
#   else:
#     result.typ = nil

# proc pyBytes*(b: cstring): Node =
#   result = Node(kind: PyBytes, s: $b, typ: T.Bytes)

# proc isList*(typ: Type): bool =
#   if typ.isNil:
#     return false
#   elif typ.kind == N.Generic and typ == T.List:
#     result = true
#   elif typ.kind == N.Compound and typ.original == T.List:
#     result = true
#   else:
#     result = false

# proc isDict*(typ: Type): bool =
#   if typ.isNil:
#     return false
#   elif typ.kind == N.Generic and typ == T.Dict:
#     result = true
#   elif typ.kind == N.Compound and typ.original == T.Dict:
#     result = true
#   else:
#     result = false

# var TRANSLATIONS = {"int": IntType, "float": FloatType, "str": StringType, "bool": BoolType}.toTable()

# proc toType*(node: Node): Type =
#   case node.kind:
#   of PyLabel:
#     if TRANSLATIONS.hasKey(node.label):
#       result = TRANSLATIONS[node.label]
#     else:
#       result = Type(kind: N.Atom, label: node.label)
#   of PyAttribute:
#     assert node[0].kind == PyLabel
#     result = Type(kind: N.Atom, label: "$1.$2" % [node[0].label, node[1].s])
#   else:
#     assert false

# proc noRec(label: string, t: Type): bool =
#   if len(label) > 0 and not t.isNil and label == t.label:
#     return false
#   return true

# proc toType*(typ: PyType): Type =
#   case typ.kind:
#   of PyTypeObject:
#     var members = initTable[string, Type]()
#     for field in typ.fields:
#       members[field.name] = toType(field.typ)
#     var isRef = false
#     for member, t in members:
#       if not noRec(typ.label, t):
#         isRef = true
#         break
#     let base = if typ.base.isNil: nil else: toType(typ.base)
#     result = Type(kind: N.Record, label: typ.label, inherited: typ.inherited, base: base, isRef: isRef, members: members)
#   of PyTypeTuple:
#     result = Type(kind: N.Tuple, elements: typ.elements.mapIt(toType(it)))
#   of PyTypeFunction:
#     result = Type(kind: N.Function, functionArgs: @[])
#     result.functionArgs = typ.args.mapIt(toType(it.typ))
#     result.returnType = if typ.returnType.isNil: T.Void else: toType(typ.returnType)
#   of PyTypeFunctionOverloads:
#     result = Type(kind: N.Overloads, label: typ.label, overloads: typ.overloads.mapIt(toType(it)))
#   of PyTypeAtom:
#     result = Type(kind: N.Atom, label: if typ.label == "str": "string" else: typ.label)
#   of PyTypeConcrete:
#     var label = typ.label
#     if label == "list":
#       label = "seq"
#     elif label == "dict":
#       label = "Table"
#     let genericBase = Type(kind: N.Generic, label: label, genericArgs: @[])
#     for z in 0..<len(typ.types):
#       genericBase.genericArgs.add(fmt"T{$z}")
#     result = Type(kind: N.Compound, args: typ.types.mapIt(toType(it)), original: genericBase)
#   of PyTypeUnion:
#     # TODO
#     result = nil
#   of PyTypeOptional:
#     # TODO
#     result = nil
#   of PyTypeGeneric:
#     result = Type(kind: N.Generic, label: typ.klass, genericArgs: toSeq(0..(typ.length - 1)).mapIt(fmt"T{$it}"))
#   of PyTypeNone:
#     result = VoidType


# let PY_NIL* = Node(kind: PyNone, children: @[], typ: VoidType)
# let PY_TRUE* = Node(kind: PyLabel, label: "true", typ: BoolType)
# let PY_FALSE* = Node(kind: PyLabel, label: "false", typ: BoolType)
# let NIM_ANY* = Type(kind: N.Any)
