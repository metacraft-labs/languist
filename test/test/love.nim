
type
  A* = ref object
type
  Love* = ref object
method b*(self: A; c: int): int
method b*(self: Love; c: string): string
method b*(self: A; c: int): int =
  return c

method b*(self: Love; c: string): string =
  return c

  a = A()
  love = Love()
echo a.b(0)
echo love.b("")
