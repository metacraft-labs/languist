class Node
  def initialize(i)
    @i = i
  end

  def to_s
    "Node" + @i.to_s
  end
end

node = Node.new(0)
puts node

