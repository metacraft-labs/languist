import macros

# dumpTree:
#   typ(self: Sequence[T]):
#     dep sequtils

#     map[T, U](y: Method[T, U]) -> Sequence[U]:         self.mapIt(y)
#     filter(y: Method[T, Bool]) -> Sequence[T]:   self.filterIt(y)
#     any_question(y: Method[T, Bool]) -> Bool:            self.anyIt(y)
#     all_question(y: Method[T, Bool]) -> Bool:            self.allIt(y)
#     push(y: T) -> Void:                          self.add(y)
#     pop() -> T:
#       var tmp = self[self.len() - 1]
#       self.delete(self.len() - 1, 1)
#       tmp
#     clear() -> Void:                             self.clear()

dumpTree: # love it! either add nim parser or parse it ourselves? maybe nim parser easier ? if in binary? but still other way type obvious!
  puts(a: Any) -> Void:                echo(a)
  String(a: Any) -> String:            text(a)

  typ(self: Any):
    to_s() -> String:                  text(self)
    to_a() -> Sequence[Any]:           self.toSeq()
    include_question(b: Any) -> Bool:  self{Sequence[Any]}.contains(b)      # FIX
    

  rewrite(x: String, y: String):
    x + y -> String:                   amp!(x, y)

  typ(self: Integer):
    times(y: Method) -> Void:
      forRange!(`y.mainArg`, 0, x, `~y.code`) 

  typ(self: Table):
    # FAITH

    dep tables

    each(y: SingleArgMethod) -> Void:
      forIn!(`y.mainArg`, x, `~y.code`)    
      
    each(y: Method) -> Void:
      forManyIn!(`y.firstArg`, `y.secondArg`, x, `~y.code`)
    
    # TODO matching more exact , but it doesnt really matter for now
    
  typ(self: Sequence[T]):
    each(y: Any) -> Void:
      forIn!(`y.mainArg`, x, `~y.code`)

    select(y: Method) -> Sequence[T]:    self.filter(y)
    first() -> T:                        index!(x, 0) # we hve to generate those so we know how, but only 
    is_one() -> Bool:                    eq!(self.len, 1) # self.len == 1

  typ(self: String):
    dep strutils

    downcase() -> String:                self.lower()
    upcase() -> String:                  self.upper()
    start_with_question() -> Bool:       self.startsWith()
    ends_with_question() -> Bool:        self.endsWith()
    just simple ro type

# # for now this ok and for other rubyies either conditional or just sections with diff package?
# languist install ruby2.5 rspec4.2


#read yaml

#and load them into idioms/ folder and save the lock file
#for the project


# ruby:
#   ~lang: 2.5
#   rspec 4.2
# python:
#   ~lang: 3.4
#   flask: 1.2
# nim:
#   ~lang: 0.19
#   ~web-nim: 0.1


# web-nim: something simple which maps more easily to sinatra/rails/phoenix/flask


# # not so hard
# # not so important
# # so putting here


# we include them on startup

# so something likle parse

