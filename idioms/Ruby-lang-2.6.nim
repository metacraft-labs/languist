# puts(a: Any) -> Void:                echo(a)

# typ(self: Any):
#   to_s() -> String:                  text(self)
#   to_a() -> Sequence[Any]:           self.toSeq()
#   include_question(b: Any) -> Bool:  self{Sequence[Any]}.contains(b)      # FIX
  

# rewrite(x: String, y: String):
#   x + y -> String:                   x.concat(y)

# typ(self: Integer):
#   times(y: Method) -> Void:
#     forRange!(`y.mainArg`, 0, x, `~y.code`) 

# typ(self: Table):
#   # FAITH

#   dep tables

#   each(y: SingleArgMethod) -> Void:
#     forIn!(`y.mainArg`, x, `~y.code`)    
    
#   each(y: Method) -> Void:
#     forManyIn!(`y.firstArg`, `y.secondArg`, x, `~y.code`)
  
#   # TODO matching more exact , but it doesnt really matter for now
  
typ(self: Sequence[T]):
  # each(y: Any) -> Void:
    # forIn!(`y.mainArg`, x, `~y.code`)

  select(y: Block[T, Bool]) -> Sequence[T]:    self.filter(y)
#   first() -> T:                        index!(x, 0) # we hve to generate those so we know how, but only  # index! is great!
#   is_one() -> Bool:                    self.len == 1

# typ(self: String):
#   dep strutils

#   downcase() -> String:                self.lower()
#   upcase() -> String:                  self.upper()
#   start_with_question() -> Bool:       self.startsWith()
#   ends_with_question() -> Bool:        self.endsWith()
