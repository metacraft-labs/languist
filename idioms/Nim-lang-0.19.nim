# echo(a: Any) -> Void:           echo(a)
# text(a: Any) -> String:         $(a)

# typ(self: Int):
#   is_positive() -> Bool:        self > 0

# # TODO
typ(self: Sequence[T]):
  dep sequtils

  # TODO
  # map[T, U](a: Block[T, U]) -> Sequence[U]:    self.mapIt(~a)
  map(a: Block[T, Any]) -> Sequence[Any]:      self.mapIt(~a)
  filter(a: Block[T, Bool]) -> Sequence[T]:    self.filterIt(~a)
  any_question(a: Block[T, Bool]) -> Bool:     self.anyIt(~a)
  all_question(a: Block[T, Bool]) -> Bool:     self.allIt(~a) 
  #  push(a: T) -> Void:                          self.add(a)
#   pop() -> T:
#     expr!(
#       defineLocalVar!("tmp", last!(self)),
#       self.delete(self.len() - 1, 1),
#       tmp)
#   clear() -> Void:                             self.clear()
#   contains(a: T) -> Bool:                      a in self

# typ(self: String):
#   dep strutils

#   lower() -> String:                           self.toLowerAscii()
#   upper() -> String:                           self.toUpperAscii()
#   capitalize() -> String:                      self.capitalizeAscii()
#   concat(a: String) -> String:                 self & a

