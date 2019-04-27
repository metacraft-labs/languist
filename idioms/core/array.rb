class RDLClass
  attr_accessor :types

  ALPHABET = ('a'..'z').to_a

  def initialize
    @types = {}
    @mapping = {Array: :Sequence, Hash: :Table, Integer: :Int}
  end

  def nowrap(name)
    @types[name] = {name: name, type_params: [], methods: {}}
  end

  def type_params(name, type_params, unknown)
    @types[name][:type_params] = type_params
  end

  def type(name, method, signature)
    if @types[name][:methods][method].nil?
      @types[name][:methods][method] = []
    end
    @types[name][:methods][method].push(parse_signature(name, method, signature))
  end

  def map_to_nim(typ, name)
    if @types[name][:type_params].include?(typ.to_sym)
      typ.upcase
    elsif [:t, :u, :v, :w, :x, :y, :z].include?(typ.to_sym)
      if @type_params.empty?
        @type_params = @types[name][:type_params].map { |it| it.to_s.upcase }
      end
      capital_type = typ.upcase
      if !@type_params.include?(capital_type)
        @type_params.push(capital_type)
      end
      capital_type
    else
      (@mapping[typ.to_sym] || typ).to_s
    end
  end
    
  def parse_tokens(type, tokens, i)
    # Range<Integer>
    # a
    typ = tokens[i].strip
    new_i = i + 1
    if typ.include?('<')
      start = typ.index('<')
      finish = typ.index('>')
      base = map_to_nim(typ[0 .. start - 1], type)
      type_arg = map_to_nim(typ[start + 1 .. finish - 1], type)
      typ = base + '[' + type_arg + ']'
    elsif i < tokens.length - 1 && tokens[i + 1] == 'or'
      # TODO more 2
      sub_tokens = [typ, tokens[i + 2]]
      new_i = i + 3
      args = sub_tokens.each_with_index.map do |j, sub|
        parse_tokens(i + j, type, tokens)[0]
      end.join(', ')
      typ = "Union[#{args}]"
    else
      typ = map_to_nim(typ, type)
    end
    [typ, new_i]
  end

  def lex(signature)
    result = []
    i = 0
    last = ''
    state = :start
    while i < signature.length
      c = signature[i]
      case state
      when :start
        if c.match(/[\w_]/)
          last = c
          state = :name
        elsif !c.match(/\s/)
          last = c
          state = :symbol
        end
      when :name
        if c.match(/[\w_\<\>]/)
          last += c
        else
          result.push(last)
          last = ''
          if !c.match(/\s/)
            last = c
            state = :symbol
          else
            state = :start
          end
        end
      when :symbol
        if c.match(/[\w_]/)
          result.push(last)
          last = c
          state = :name
        elsif c.match(/\s/)
          result.push(last)
          last = ''
          state = :start
        else
          last += c
        end
      end
      p "#{c} #{state}"
      i += 1
    end
    if !last.empty?
      result.push(last)
    end
    result
  end

  def parse_signature(type, method, signature)
    # << ((t) -> Array<t>)
    # `<<`(a: T) -> Sequence[T]: self.`<<`(a) # TODO

    # [] ((Range<Integer>) -> Array<t>)
    # `[]`(a: Range[Int]) -> Sequence[T]: self.`[]`(a) # TODO

    tokens = lex(signature)
    result = {}

    @type_params = []
    result[:input] = {name: method, args: [], return_type: nil}
    result[:output] = {name: method, receiver: !type.nil? ? 'self' : '', args: []}
    p tokens
    i = 0
    j = 0
    k = 0
    while j < tokens.length - 2
      token = tokens[j]
      if token == '(' || token == ')' || token == '()'
        j += 1
        next
      elsif token == '{'
        next_j = tokens[j .. -3].index('}') + j
        i = j + 1
        in_arg = false
        tokens[j + 1.. next_j - 1].each do |child|
          p "child #{child}"
          block_args = []
          block_type = ''
          if child == '('
            in_arg = true
            i += 1
          elsif child == ')' || child == '()'
            in_arg = false
            i += 1
          elsif child != '->' && child != ','
            if in_arg
              block_arg, i = parse_tokens(type, tokens, i) # TODO: or
            else
              return_type, i = parse_tokens(type, tokens, i) # TODO: or
              block_params = block_args + [return_type]
              params = block_params.join(', ')
              block_type = "Block[#{params}]"
              result[:input][:args].push([ALPHABET[k], block_type])
              result[:output][:args].push(ALPHABET[k])
              break
            end
          else
            i += 1
          end
        end
        j = next_j + 1
        i = j
        next
      end
      new_arg, i = parse_tokens(type, tokens, i)
      result[:input][:args].push([ALPHABET[k], new_arg])
      result[:output][:args].push(ALPHABET[k])
      k += 1
      j += 1
    end
    # FAITH
    result[:input][:return_type] = parse_tokens(type, tokens, tokens.length - 1)[0]
    result[:input][:type_params] = @type_params
    result
  end

  def rdl_alias(name, method_name, old_name)
    @types[name][method_name] = @types[name][old_name]
  end


  def generate_input(input)
    name = input[:name].to_s
    if !name.between?('a', 'z')
      name = "`#{name}`"
    elsif name[-1] == '?'
      name = "#{name[0 .. -2]}_question"
    elsif name[-1] == '!'
      name = "#{name[0 .. -2]}_bang"
    end
    args = input[:args].map { |it| "#{it[0]}: #{it[1]}" }.join(', ')
    return_type = input[:return_type]
    type_params = !input[:type_params].empty? ? '[' + input[:type_params].join(', ') + ']' : ''
    "#{name}#{type_params}(#{args}) -> #{return_type}"
  end

  def generate_output(output)
    name = output[:name].to_s
    if !name.between?('a', 'z')
      name = "`#{name}`"
    elsif name[-1] == '?'
      name = "#{name[0 .. -2]}_quftion"
    elsif name[-1] == '!'
      name = "#{name[0 .. -2]}_bang"
    end
    receiver = output[:receiver] != '' ? "#{output[:receiver]}." : ''
    args = output[:args].join(', ')
    "#{receiver}#{name}(#{args})"
  end

  def generate_idioms
    result = ''
    indent = 0
    @types.each do |name, typ|
      full_name = map_to_nim(name, name)
      if !typ[:type_params].empty?
        full_name += '[' + typ[:type_params].map { |it| it.to_s.upcase }.join(', ') + ']'
      end
      result += "typ(#{full_name}):\n"
      indent = 2
      typ[:methods].each do |method, idioms|
        idioms.each do |idiom|
          result += "#{' ' * indent}#{(generate_input(idiom[:input]) + ':').ljust(48)}#{generate_output(idiom[:output])}\n"
        end
      end
      indent = 0
    end
    indent = 0
    result
  end

  def save_idioms(path)
    File.write(path, generate_idioms)
  end
end

RDL = RDLClass.new

RDL.nowrap :Array

RDL.type_params :Array, [:t], :all?

RDL.type :Array, :<<, '(t) -> Array<t>'
RDL.type :Array, :[], '(Range<Integer>) -> Array<t>'
RDL.type :Array, :[], '(Integer or Float) -> t'
RDL.type :Array, :[], '(Integer, Integer) -> Array<t>'
RDL.type :Array, :&, '(Array<u>) -> Array<t>'
RDL.type :Array, :*, '(Integer) -> Array<t>'
RDL.type :Array, :*, '(String) -> String'
RDL.type :Array, :+, '(Enumerable<u>) -> Array<u or t>'
RDL.type :Array, :+, '(Array<u>) -> Array<u or t>'
RDL.type :Array, :-, '(Array<u>) -> Array<u or t>'
RDL.type :Array, :slice, '(Range<Integer>) -> Array<t>'
RDL.type :Array, :slice, '(Integer) -> t'
RDL.type :Array, :slice, '(Integer, Integer) -> Array<t>'
RDL.type :Array, :[]=, '(Integer, t) -> t'
RDL.type :Array, :[]=, '(Integer, Integer, t) -> t'
# RDL.type :Array, :[]=, '(Integer, Integer, Array<t>) -> Array<t>'
# RDL.type :Array, :[]=, '(Range, Array<t>) -> Array<t>'
RDL.type :Array, :[]=, '(Range<Integer>, t) -> t'
RDL.type :Array, :assoc, '(t) -> Array<t>'
RDL.type :Array, :at, '(Integer) -> t'
RDL.type :Array, :clear, '() -> Array<t>'
RDL.type :Array, :map, '() {(t) -> u} -> Array<u>'
RDL.type :Array, :map, '() -> Enumerator<t>'
RDL.type :Array, :map!, '() {(t) -> u} -> Array<u>'
RDL.type :Array, :map!, '() -> Enumerator<t>'
RDL.type :Array, :collect, '() { (t) -> u } -> Array<u>'
RDL.type :Array, :collect, '() -> Enumerator<t>'
RDL.type :Array, :combination, '(Integer) { (Array<t>) -> %any } -> Array<t>'
RDL.type :Array, :combination, '(Integer) -> Enumerator<t>'
RDL.type :Array, :push, '(*t) -> Array<t>'
RDL.type :Array, :compact, '() -> Array<t>'
RDL.type :Array, :compact!, '() -> Array<t>'
RDL.type :Array, :concat, '(Array<t>) -> Array<t>'
RDL.type :Array, :count, '() -> Integer'
RDL.type :Array, :count, '(t) -> Integer'
RDL.type :Array, :count, '() { (t) -> %bool } -> Integer'
RDL.type :Array, :cycle, '(?Integer) { (t) -> %any } -> %any'
RDL.type :Array, :cycle, '(?Integer) -> Enumerator<t>'
RDL.type :Array, :delete, '(u) -> t'
RDL.type :Array, :delete, '(u) { () -> v } -> t or v'
RDL.type :Array, :delete_at, '(Integer) -> Array<t>'
RDL.type :Array, :delete_if, '() { (t) -> %bool } -> Array<t>'
RDL.type :Array, :delete_if, '() -> Enumerator<t>'
RDL.type :Array, :drop, '(Integer) -> Array<t>'
RDL.type :Array, :drop_while, '() { (t) -> %bool } -> Array<t>'
RDL.type :Array, :drop_while, '() -> Enumerator<t>'
RDL.type :Array, :each, '() -> Enumerator<t>'
RDL.type :Array, :each, '() { (t) -> %any } -> Array<t>'
RDL.type :Array, :each_index, '() { (Integer) -> %any } -> Array<t>'
RDL.type :Array, :each_index, '() -> Enumerator<t>'
RDL.type :Array, :empty?, '() -> %bool'
RDL.type :Array, :fetch, '(Integer) -> t'
RDL.type :Array, :fetch, '(Integer, u) -> u'
RDL.type :Array, :fetch, '(Integer) { (Integer) -> u } -> t or u'
RDL.type :Array, :fill, '(t) -> Array<t>'
RDL.type :Array, :fill, '(t, Integer, ?Integer) -> Array<t>'
RDL.type :Array, :fill, '(t, Range<Integer>) -> Array<t>'
RDL.type :Array, :fill, '() { (Integer) -> t } -> Array<t>'
RDL.type :Array, :fill, '(Integer, ?Integer) { (Integer) -> t } -> Array<t>'
RDL.type :Array, :fill, '(Range<Integer>) { (Integer) -> t } -> Array<t>'
RDL.type :Array, :flatten, '() -> Array<%any>' # Can't give a more precise RDL.type
RDL.type :Array, :index, '(u) -> Integer'
RDL.type :Array, :index, '() { (t) -> %bool } -> Integer'
RDL.type :Array, :index, '() -> Enumerator<t>'
RDL.type :Array, :first, '() -> t'
RDL.type :Array, :first, '(Integer) -> Array<t>'
RDL.type :Array, :include?, '(u) -> %bool'
RDL.type :Array, :initialize, '() -> self'
RDL.type :Array, :initialize, '(Integer) -> self'
RDL.type :Array, :initialize, '(Integer, t) -> self<t>'
RDL.type :Array, :insert, '(Integer, *t) -> Array<t>'
RDL.type :Array, :inspect, '() -> String'
RDL.type :Array, :join, '(?String) -> String'
RDL.type :Array, :keep_if, '() { (t) -> %bool } -> Array<t>'
RDL.type :Array, :last, '() -> t'
RDL.type :Array, :last, '(Integer) -> Array<t>'
RDL.type :Array, :member, '(u) -> %bool'
RDL.type :Array, :length, '() -> Integer'
RDL.type :Array, :permutation, '(?Integer) -> Enumerator<t>'
RDL.type :Array, :permutation, '(?Integer) { (Array<t>) -> %any } -> Array<t>'
RDL.type :Array, :pop, '(Integer) -> Array<t>'
RDL.type :Array, :pop, '() -> t'
RDL.type :Array, :product, '(*Array<u>) -> Array<Array<t or u>>'
RDL.type :Array, :rassoc, '(u) -> t'
RDL.type :Array, :reject, '() { (t) -> %bool } -> Array<t>'
RDL.type :Array, :reject, '() -> Enumerator<t>'
RDL.type :Array, :reject!, '() { (t) -> %bool } -> Array<t>'
RDL.type :Array, :reject!, '() -> Enumerator<t>'
RDL.type :Array, :repeated_combination, '(Integer) { (Array<t>) -> %any } -> Array<t>'
RDL.type :Array, :repeated_combination, '(Integer) -> Enumerator<t>'
RDL.type :Array, :repeated_permutation, '(Integer) { (Array<t>) -> %any } -> Array<t>'
RDL.type :Array, :repeated_permutation, '(Integer) -> Enumerator<t>'
RDL.type :Array, :reverse, '() -> Array<t>'
RDL.type :Array, :reverse!, '() -> Array<t>'
RDL.type :Array, :reverse_each, '() { (t) -> %any } -> Array<t>'
RDL.type :Array, :reverse_each, '() -> Enumerator<t>'
RDL.type :Array, :rindex, '(u) -> t'
RDL.type :Array, :rindex, '() { (t) -> %bool } -> Integer'
RDL.type :Array, :rindex, '() -> Enumerator<t>'
RDL.type :Array, :rotate, '(?Integer) -> Array<t>'
RDL.type :Array, :rotate!, '(?Integer) -> Array<t>'
RDL.type :Array, :sample, '() -> t'
RDL.type :Array, :sample, '(Integer) -> Array<t>'
RDL.type :Array, :select, '() { (t) -> %bool } -> Array<t>'
RDL.type :Array, :select, '() -> Enumerator<t>'
RDL.type :Array, :select!, '() { (t) -> %bool } -> Array<t>'
RDL.type :Array, :select!, '() -> Enumerator<t>'
RDL.type :Array, :shift, '() -> t'
RDL.type :Array, :shift, '(Integer) -> Array<t>'
RDL.type :Array, :shuffle, '() -> Array<t>'
RDL.type :Array, :shuffle!, '() -> Array<t>'
RDL.rdl_alias :Array, :size, :length
RDL.rdl_alias :Array, :slice, :[]
RDL.type :Array, :slice!, '(Range<Integer>) -> Array<t>'
RDL.type :Array, :slice!, '(Integer, Integer) -> Array<t>'
RDL.type :Array, :slice!, '(Integer or Float) -> t'
RDL.type :Array, :sort, '() -> Array<t>'
RDL.type :Array, :sort, '() { (t,t) -> Integer } -> Array<t>'
RDL.type :Array, :sort!, '() -> Array<t>'
RDL.type :Array, :sort!, '() { (t,t) -> Integer } -> Array<t>'
RDL.type :Array, :sort_by!, '() { (t) -> u } -> Array<t>'
RDL.type :Array, :sort_by!, '() -> Enumerator<t>'
RDL.type :Array, :take, '(Integer) -> Array<t>'
RDL.type :Array, :take_while, '() { (t) ->%bool } -> Array<t>'
RDL.type :Array, :take_while, '() -> Enumerator<t>'
RDL.type :Array, :to_a, '() -> Array<t>'
RDL.type :Array, :to_ary, '() -> Array<t>'
RDL.rdl_alias :Array, :to_s, :inspect
RDL.type :Array, :transpose, '() -> Array<t>'
RDL.type :Array, :uniq, '() -> Array<t>'
RDL.type :Array, :uniq!, '() -> Array<t>'
RDL.type :Array, :unshift, '(*t) -> Array<t>'
RDL.type :Array, :values_at, '(*Range<Integer> or Integer) -> Array<t>'
RDL.type :Array, :zip, '(*Array<u>) -> Array<Array<t or u>>'
RDL.type :Array, :|, '(Array<u>) -> Array<t or u>'
RDL.type :Array, :collect, '() { (t) -> u } -> Array<u>'

p RDL.types[:Array][:methods][:collect]

RDL.save_idioms('array.nim')

