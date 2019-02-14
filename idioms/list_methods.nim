import tables, strutils, sequtils, strformat
import idioms_dsl, ../types, ../ast_dsl, ../core

# methods: we have auto variables
# receiver which is equivalent to the object that receives the method
# (bit smalltalk naming)
# method(argName: ArgType <Type>*) => methodName: <Type> we assume it's receiver.methodName(argName*)
# method(argName: ArgType <Type>*) => nimfunction(argName*): <Type>
# method(argName: ArgType <Type>*): handler
# nimfunction can be <stuff>.<method>(<args>) or <function>(<args>)
# dependencies = <a table> method: seq[lib]
# dependenciesAll = seq[lib] when all methods need a lib
# dependenciesIgnore = seq[lib] blacklist methods that doesnt need a lib

builtin("Sequence", Lang.Nim):
  push(e: E) =>                           add: Void
  insert(i: Int, e: E):
    call(attribute(receiver, "insert"), @[list(@[e]), i], Void)

  dependenciesAll = @["sequtils"]
  dependenciesIgnore = @["push"]

builtin("Sequence", Lang.Ruby):
  push(e: E) =>                           push
  insert(i: Int, e: E) =>                 insert
  map(a: Method[E, T]):
    ruby_block_call(attribute(receiver, "map"), a)
  

# or

builtin("Sequence", Lang.Nim):
  @[
    (methodCall("push").args(e=E),              methodCall("add", Void)),
    (methodCall("insert").args(i=Int, e=E),     call(attribute(receiver, "insert"), @[list(@[e]), i], Void))]

  dependenciesAll = @["sequtils"]
  dependenciesIgnore = @["push"]

  
  
