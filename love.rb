class A
  def b(c)
    puts c
  end
end

class Love
  def b(c)
  	puts c
  end
end

a = A.new
love = Love.new

a.b(0)
love.b("")

[2].map { |e| p e }

def e
  raise "e"
end


# e()
# begin
#   e()
# rescue
#   puts 0
# end
