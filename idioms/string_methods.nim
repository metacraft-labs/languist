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

builtin(T.String):
  # https://docs.python.org/3.6/library/string.html

  capitalize() =>                           capitalizeAscii: T.String
  
  center(width: T.Int) =>                   center: T.String
  
  count(sub: T.String) =>                   count: T.String
  
  count(sub: T.String, start: T.Int) =>     count: T.String
  
  count(sub: T.String,
        start: T.Int,
        finish: T.Int) =>                   count: T.String

  endswith(suffix: T.String) =>             endsWith: T.String
  
  endswith(suffix: T.String,
           start: T.Int):
    call(attribute(subscript(receiver, slice(start)), "endsWith"), @[suffix], T.String)

  endswith(suffix: T.String,
           start: T.Int,
           finish: T.Int):
    call(attribute(subscript(receiver, slice(start, finish)), "endsWith"), @[suffix], T.String)

  expandTabs() =>                           expandTabs: T.String

  expandTabs(tabsize: T.Int) =>             expandTabs: T.String

  find(sub: T.String) =>                    find: T.String

  find(sub: T.String, start: T.Int) =>      find: T.String

  find(sub: T.String,
       start: T.Int,
       finish: T.Int) =>                    find: T.String

  index(sub: T.String) =>                   findOrRaise: T.String

  index(sub: T.String, start: T.Int) =>     findOrRaise: T.String

  index(sub: T.String,
        start: T.Int,
        finish: T.Int) =>                   findOrRaise: T.String

  isalnum() =>                              isAlphaNumeric: T.Bool

  isalpha() =>                              isAlphaAscii: T.Bool

  isdecimal() =>                            isDigit: T.Bool

  isdigit() =>                              isDigit: T.Bool

  isspace() =>                              isSpaceAscii: T.Bool

  istitle() =>                              isTitle: T.Bool

  islower() =>                              isLowerAscii: T.Bool

  isupper() =>                              isUpperAscii: T.Bool

  join(iterable: T.List) =>                 iterable.join(receiver): T.String

  # ljust(width: T.Int):
  #   binop(receiver, add(), call(ident("repeat"), @[PythonNode(kind: PyChar, c: ' '), width], T.String), T.String)

  # ljust(width: T.Int, fillchar: T.String):
  #   binop(receiver, add(), call(ident("repeat"), @[fillchar, width], T.String), T.String)

  lower() =>                                toLowerAscii: T.String

  lstrip() =>                               receiver.strip(PY_TRUE, PY_FALSE): T.String

  lstrip(chars: T.Seq) =>                   receiver.strip(PY_TRUE, PY_FALSE, chars): T.String

  split(s: T.String) =>                     split: T.List[T.String]

  dependenciesAll = @["strutils"]

  dependencies = {
    "expandTabs": @["python_methods"],
    "findOrRaise": @["python_methods"],
    "isTitle": @["unicode"]
  }.toTable()
