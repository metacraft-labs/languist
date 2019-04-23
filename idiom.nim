import macros

dumpTree:
  typ(self: Sequence[T]):
    dep sequtils

    map[T, U](y: Method[T, U]) -> Sequence[U]:         self.mapIt(y)
    filter(y: Method[T, Bool]) -> Sequence[T]:   self.filterIt(y)
    any_question(y: Method[T, Bool]) -> Bool:            self.anyIt(y)
    all_question(y: Method[T, Bool]) -> Bool:            self.allIt(y)
    push(y: T) -> Void:                          self.add(y)
    pop() -> T:
      var tmp = self[self.len() - 1]
      self.delete(self.len() - 1, 1)
      tmp
    clear() -> Void:                             self.clear()


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

