class A
  def b(c)
    c
  end
end

class Love
  def b(c)
  	c
  end
end

a = A.new
love = Love.new

puts a.b(0)
puts love.b("")

