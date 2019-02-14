import tables, strutils, sequtils, strformat
import idioms_dsl, ../python_types, ../nim_types, ../python_ast, ../ast_dsl, ../core, ../dependency

# methods: we have magical variables
# receiver which is equivalent to the python object that receives the method
# (bit smalltalk naming)
# pymethod(argName: ArgType <Type>*) => methodName: <Type> we assume it's receiver.methodName(argName*)
# pymethod(argName: ArgType <Type>*) => nimfunction(argName*): <Type>
# pymethod(argName: ArgType <Type>*): handler
# nimfunction can be <stuff>.<method>(<args>) or <function>(<args>)
# dependencies = <a table> method: seq[lib]
# dependenciesAll = seq[lib] when all methods need a lib
# dependenciesIgnore = seq[lib] blacklist methods that doesnt need a lib

builtin(T.Dict[K, V]):
  # https://docs.python.org/3.6/tutorial/datastructures.html#more-on-lists

  clear() =>                            clear: T.Void
  
  get(key: K):
    # receiver[key] if receiver.hasKey(key) else None
    ifExp(
      call(attribute(receiver, "hasKey"), @[key], T.Bool),
      subscript(receiver, key, V),
      PY_NIL,
      typ=V)

  #dependenciesAll = @["sequtils"]

  #dependenciesIgnore = @["append"]

# echo builtinMethods
