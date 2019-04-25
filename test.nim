import tables

var a: Table[string, int]
a["a"] = 0

var b: Table[string, int] = initTable[string, int]()
b["a"] = 0

echo a, a.hasKey("a")
echo b, b.hasKey("a")
