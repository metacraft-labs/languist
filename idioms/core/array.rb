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
    elsif @[:t, :u, :v, :w, :x, :y, :z].include?(:typ.to_sym)

        @types[name][:methods][method]
      typ.upcase
    else
      (@mapping[typ.to_sym] || typ).to_s
    end
  end
    
  def parse_token(i, type, token)
    # Range<Integer>
    # a
    result = []
    tokens = token.split(',').map(&:strip)
    tokens.each do |token_child|
      typ = token_child
      if typ.include?('<')
        start = typ.index('<')
        finish = typ.index('>')
        base = map_to_nim(typ[0 .. start - 1], type)
        type_arg = map_to_nim(typ[start + 1 .. finish - 1], type)
        typ = base + '[' + type_arg + ']'
      elsif typ.include?(' or ')
        sub_tokens = typ.split(' or ').map(&:strip)
        args = sub_tokens.map do |sub|
          parse_token(0, type, sub)[0][1]
        end.join(', ')
        typ = "Union[#{args}]"
      else
        typ = map_to_nim(typ, type)
      end
      arg = [ALPHABET[i], typ]
      i += 1
      result.push(arg)
    end
    result
  end

  def parse_signature(type, method, signature)
    # << ((t) -> Array<t>)
    # `<<`(a: T) -> Sequence[T]: self.`<<`(a) # TODO

    # [] ((Range<Integer>) -> Array<t>)
    # `[]`(a: Range[Int]) -> Sequence[T]: self.`[]`(a) # TODO

    tokens = signature.split(' -> ')
    result = {}

    @type_args = []
    result[:input] = {name: method, args: [], return_type: nil}
    result[:output] = {name: method, receiver: !type.nil? ? 'self' : '', args: []}
    if tokens[0].start_with?('(') && tokens[0].end_with?(')')
      tokens[0] = tokens[0][1 .. -2]
    end
    p tokens
    i = 0
    tokens[0 .. -2].each do |token|
      new_args = parse_token(i, type, token)
      result[:input][:args] += new_args
      old_i = i
      i += new_args.length
      if i > old_i
        result[:output][:args] += ALPHABET[old_i .. i - 1]
      end
    end
    # FAITH
    result[:input][:return_type] = parse_token(0, type, tokens[-1])[0][1]
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
    "#{name}(#{args}) -> #{return_type}"
  end

  def generate_output(output)
    name = output[:name].to_s
    if !name.between?('a', 'z')
      name = "`#{name}`"
    elsif name[-1] == '?'
      name = "#{name[0 .. -2]}_question"
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

p RDL.types[:Array][:methods][:[]]

RDL.save_idioms('array.nim')

