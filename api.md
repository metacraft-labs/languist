# Enumerable

# Sequence
  map(Block[T, U]) => Sequence[U]
  filter(Block[T, Bool]) => Sequence[T]

# Table
  each

Ruby

receiver = Sequence[T]
map(b: Block[T, U]) => map(Block[T, U])
select(b: Block[T, U]) => select(Block[T, U])
