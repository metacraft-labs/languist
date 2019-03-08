require 'json'
require 'parser/current'
require 'ast'
require 'set'

$trace = []
$lines = []
$inter_traces = {}
$inter_types = {}
$call_lines = []
$current_block = ""
$name_pattern = ENV["RB2NIM_FILENAME"]
$target_folder = ENV["RB2NIM_TARGET_FOLDER"]

def trace_calls(data)
  $call_lines.push(data.lineno)
  if $inter_traces[data.path][:method_lines].key?(data.lineno)
    if $inter_traces[data.path][:method_lines][data.lineno][:kind] == :NodeMethod && data.event != :b_call ||
       $inter_traces[data.path][:method_lines][data.lineno][:kind] == :Block && data.event == :b_call
      $inter_traces[data.path][:method_lines][data.lineno][:args].each do |arg|
        puts "ARG #{arg}"
        if arg[:label] == :self
          arg[:typ] = load_type(data.binding.receiver)
        else
          begin
            arg[:typ] = load_type(data.binding.local_variable_get(arg[:label]))
          rescue
            arg[:typ] = {kind: :Simple, label: "Void"}
          end
        end
      end
    end
  end
end

$t = TracePoint.new(:call, :c_call, :b_call) do |tp|
  if tp.method_id != :method_added && tp.path.include?("rubocop") && tp.path.include?($name_pattern)
    a = 0
  else
    next
  end
  # TODO require require_relative
  if ![:new, :initialize, :enable, :disable, :require_relative, :require].include?(tp.method_id) #tp.path != "tracing.rb" && 
    p tp
    if !$inter_traces.key?(tp.path)
      $inter_traces[tp.path] = generate_ast(tp.path)
    end
    trace_calls(tp)
  end
end

#tp.defined_class != TracePoint && tp.defined_class != Kernel && tp.method_id != :disable && tp.method_id != :enable && tp.method_id != :inherited && tp.defined_class != Class && tp.method_id != :method_added &&
    #!tp.path.include?("core_ext")
    
#tp.method_id != :new && tp.method_id != :initialize && tp.method_id != :enable && tp.method_id != :disable && caller.length > 1 && 
     #tp.path != "tracing.rb" && !tp.path.include?("did_you_mean") &&
     #!tp.path.include?("core_ext")
    
$t2 = TracePoint.new(:return, :c_return, :b_return) do |tp|
  if tp.method_id != :method_added && tp.path.include?("rubocop") && tp.path.include?($name_pattern)
    p tp    
  else
    next
  end
  if ![:new, :initialize, :enable, :disable].include?(tp.method_id) 
    path, line, *_ = caller[1].split(':')
    line = line.to_i
    path = tp.path
    line = tp.lineno
    typ = load_type(tp.return_value)
    if $inter_traces.key?(path) && $inter_traces[path][:lines].key?(line)
      $inter_traces[path][:lines][line].each do |a|
        if a[:children][1][:kind] == :Variable && a[:children][1][:label] == tp.method_id
          a[:typ] = typ
        end
      end
    end
  
    if $call_lines.length > 0
      method_line = $call_lines.pop
      if $inter_traces.key?(tp.path) && $inter_traces[tp.path][:method_lines].key?(method_line)
        kind = $inter_traces[tp.path][:method_lines][method_line][:kind]
        is_block = tp.event == :b_return
        if kind == :Block && is_block || kind == :NodeMethod && !is_block
          $inter_traces[tp.path][:method_lines][method_line][:return_type] = typ
        end
      end
    end
  end
end

t3 = TracePoint.new(:raise) do |tp|
  exception = tp.raised_exception
  $trace[-1][-1] = exception
end

  



class Type
  attr_reader :kind, :args, :return_type, :label
end

TABLE_TYPE = {kind: :Generic, label: "Table", genericArgs: ["K", "V"]}
$processing = {}
# object types

def load_type(arg)
  if arg.class.nil?
    return {kind: :Simple, label: "Void"}
  end
  
  #$t.disable
  #$t2.disable
  
  klass = arg.class
  # puts "TYPE #{arg} #{klass}"
  if !$processing.key?(klass.name)
    $processing[klass.name] = []
    variables = arg.instance_variables.map do |a|
      load_type(arg.instance_variable_get(a)).tap { |b| b[:fieldLabel] = a.to_s[1 .. -1] }
    end
    $processing[klass.name] = variables
  else
    return {kind: :Simple, label: klass.name.to_sym}
  end
  res = if klass == Integer
    {kind: :Simple, label: "Int"}
  elsif klass == NilClass
    {kind: :Simple, label: "Void"}
  elsif klass == String
    {kind: :Simple, label: "String"}
  elsif klass == TrueClass || klass == FalseClass
    {kind: :Simple, label: "Bool"}
  elsif klass == Hash
    if arg.length == 0
      key = {kind: :Simple, label: "Void"}
      value = key
    else
      arg.each do |k, v|
        key = load_type(k)
        value = load_type(v)
        break
      end
    end
    {kind: :Compound, args: [key, value], original: TABLE_TYPE}
  elsif klass == Symbol
    {kind: :Simple, label: "Symbol"}
  else
    {kind: :Simple, label: klass.name}
  end

  # Praise the Lord!





  if klass.is_a?(Class) && variables.length > 0
    # ok for now
    if !$inter_types[res[:label].to_sym]
      label = res[:label].to_sym
      res[:kind] = :Object
      res[:fields] = [] # TODO inheritance variables
      $inter_types[label] = res
      if label.to_s.include?('::')
        p label.to_s.split('::')[-1].to_sym
        $inter_types[label.to_s.split('::')[-1].to_sym] = res
      end
      res = {kind: :Simple, label: label} # Praise the Lord!
    end
    # far simpler to just move all the classes to %types: easier to process and search
  end
  
  #$t.enable
  #$t2.enable 
  res
end

def load_method(args, return_type)
  {kind: :NodeMethod, args: args.map { |arg| load_type(arg) }, return_type: load_type(return_type)}
end


def union(left, right)
  case left[:kind]
  when :Method
    same = left[:args] == right[:args] and left[:return_type] == right[:return_type]
    if same
      [left, true]
    else
      [{kind: :MethodOverload, overloads: [left, right]}, false]
    end
  when :MethodOverload
    p left
  when :Simple
    if left[:label] == right[:label]
      [left, true]
    else
      [{kind: :Union, variants: [left, right]}, true]
    end
  end
end

def write(inputs)
  traces = {}
  paths = {}
  methods = {}
  i = 0
  method_stream = []
  inputs.each do |input|
    id = "#{input[2]}.#{input[3]}"
    if !methods.key?(id)
      methods[id] = []
    end
    if input[5] == :input
      methods[id].push([input, []])
      method_stream.push(methods[id][-1])
    else
      methods[id][-1][1] = input
      methods[id].pop
    end
  end

  method_stream.each do |input, result|
    if result.length == 0
      next
    end
    id = "#{input[2]}.#{input[3]}"
    if traces.key?(id)
      traces[id], _ = union(traces[id], load_method(input[4], result[4]), nil)
    else
      traces[id] = load_method(input[4], result[4])
    end
    if not paths.key?(input[0])
      paths[input[0]] = []
    end
    paths[input[0]].push(id)
  end

  [traces, paths]
end

INTER_CLASS = 0
INTER_METHOD = 1
INTER_CALL = 2
INTER_VARIABLE = 3

PATTERN_STDLIB = {puts: -> a { {kind: INTER_CALL, children: [{kind: INTER_VARIABLE, label: "echo"}] + a , typ: {kind: :Nil} } } }

KINDS = {lvasgn: :Assign, array: :Sequence, hash: :Table, begin: :Code, dstr: :Docstring}

class InterTranslator
  def initialize(ast)
    @ast = ast
  end

  def process
    $t.disable # TODO same event
    $t2.disable
    res = {imports: [], main: [], classes: [], lines: {}, method_lines: {}}
    @inter_ast = res
    children = [@ast]
    local = @ast
    if @ast.type == :begin
      children = @ast.children
      local = children[-1]
    end
    # HACK: improve
    
    if local.type == :module
      while local.children.length >= 2 && local.children[1].type == :module
        local = local.children[1]
      end
      children = local.children[1 .. -1]
    end
    children.each do |it|
      if it.type == :class
        @current_class = it.children[0].children[1].to_s
        res[:classes].push(process_node it)
        @current_class = ''
      elsif it.type == :send && it.children[0].nil? && it.children[1] == :require
        res[:imports].push(it.children[2].children[0])
      else
        # p it, process_node(it)
        res[:main].push(process_node it)
      end
    end
    $t.enable
    $t2.enable
    res
  end

  def get_kind(type)
    if KINDS.key?(type)
      KINDS[type]
    else
      :"Ruby#{type.capitalize}"
    end
  end

  def process_node(node)
    if node.class == Parser::AST::Node
      if respond_to?(:"process_#{node.type}")
        return send :"process_#{node.type}", node
      end
      begin
        line = node.loc.line
        column = node.loc.column
      rescue
        line = -1
        column = -1
      end
      if node.type == :def
        value = {kind: :NodeMethod, label: {typ: :Variable, label: node.children[0]}, args: [], code: [], typ: nil, return_type: nil}
        value[:args] = [{kind: :Variable, label: :self, typ: nil}] + node.children[1].children.map { |it| process_node it }
        value[:code] = node.children[2 .. -1].map { |it| process_node it }
        @inter_ast[:method_lines][line] = value
        return value.tap { |t| t[:line] = line; t[:column] = column }
      elsif node.type == :block
        value = {kind: :Block, label: {typ: :Variable, label: ""}, args: [], code: [], typ: nil, return_type: nil}
        value[:args] = node.children[1].children.map { |it| process_node it }
        value[:code] = node.children[2 .. -1].map { |it| process_node it }
        @inter_ast[:method_lines][line] = value
        child = process_node node.children[0]
        child[:children].push(value)
        return child.tap { |t| t[:line] = line; t[:column] = column }
      end
      value = {kind: get_kind(node.type), children: node.children.map { |it| process_node it }, typ: nil}
      if node.type == :send
        if value[:children][0][:kind] == :RubyConst && value[:children][1][:kind] == :Variable && value[:children][1][:label] == :new
          value = {kind: :New, children: [{kind: :Variable, label: value[:children][0][:label]}] + value[:children][2 .. -1]}          
        end
        value[:children][1][:kind] = :Variable
        value[:children][1][:label] = value[:children][1][:text]
        value[:children][1].delete :text
        if !@inter_ast[:lines].key?(line)
          @inter_ast[:lines][line] = []
        end
        @inter_ast[:lines][line].push(value)
        value
      else
        value
      end.tap { |t| if !node.nil?; t[:line] = line; t[:column] = column; end }
    elsif node.class == Integer
      {kind: :Int, i: node, typ: nil}
    elsif node.class == String
      {kind: :String, text: node, typ: nil}
    elsif node.class == Symbol
      {kind: :Symbol, text: node, typ: nil}
    elsif node.nil?
      {kind: :Nil, typ: nil}
    end
  end

  def process_const(node)
    {kind: :RubyConst, label: node.children[1]}
  end

  def process_int(node)
    {kind: :Int, i: node.children[0]}
  end

  def process_str(node)
    {kind: :String, text: node.children[0]}
  end

  def process_dstr(node)
    {kind: :Docstring, text: node.children.map { |it| p it; '' }.join }
  end

  def process_sym(node)
    {kind: :Symbol, text: node.children[0]}
  end  
  
  def process_ivar(node)
    {kind: :Attribute, children: [{kind: :Self}, {kind: :String, text: node.children[0][1 .. -1]}]}
  end

  def process_lvar(node)
    {kind: :Variable, label: node.children[0]}
  end

  def process_ivasgn(node)
    {kind: :Assign, children: [{kind: :Attribute, children: [{kind: :Self}, {kind: :String, text: node.children[0][1 .. -1]}]}, process_node(node.children[1])]}
  end

  def process_lvasgn(node)
    {kind: :Assign, children: [{kind: :Variable, label: node.children[0]}, process_node(node.children[1])]}
  end

  def process_casgn(node)
    value = node.children[-1]
    if value.type == :send && value.children[-1] == :freeze
      value = value.children[0]
    end
    # how to do it when global and several have the same name? label = "#{@current_class}#{node.children[1]}"
    label = node.children[1]
    {kind: :Assign, children: [{kind: :Variable, label: label}, process_node(value)], declaration: :Const}
  end

  def process_nil(node)
    {kind: :Nil}
  end

  def process_arg(node)
    {kind: :Variable, label: node.children[0]}
  end

  def process_module(node)
    process_node(node.children[1])
  end

  def process_masgn(node)
    if node.children[0].children.length == 1
      right = {kind: :Index, children: [process_node(node.children[1].children[0].children[0]), {kind: :Int, i: 0}]}
      {kind: :Assign, children: [{kind: :Variable, label: process_node(node.children[0].children[0].children[1])}, right]}
    else
      {kind: :Nil}
    end
  end
end

class InterProcessor
  include AST::Processor::Mixin

  def initialize(traces, methods, inter_module)
    @traces = traces
    @methods = methods
    @path = ''
    @inter_module = inter_module
  end

  def on_class(node)
    old_path = @path
    @path += node.children[0].children[1].to_s
    @inter_module[:classes].push({kind: INTER_CLASS, label: node.children[0].children[1].to_s, fields: {}, methods: {}})
    old_class = @inter_class
    @inter_class = @inter_module[:classes][-1]
    node.updated(nil, process_all(node))
    @inter_class = old_class
    @path = old_path

  end

  def load_args(node, traces)
    node.children.each_with_index.map { |it, i| {kind: INTER_VARIABLE, label: it.children[0].to_s, typ: traces[:args][i]}}
  end

  def on_def(node)
    id = "#{@path}.#{node.children[0].to_s}"
    if @methods.include?(id) && @traces.key?(id)
      args = load_args(node.children[1], @traces[id])
      new_node = {kind: INTER_METHOD, label: node.children[0].to_s, id: id, args: args, code: [], return_type: @traces[id][:return_type], raises: []}
      if !@traces[id][-1].nil?
        new_node[:raises].push({kind: INTER_VARIABLE, label: @traces[id][-1].class.to_s})
      end
      @inter_method = new_node
      process(node.children[2])
      @inter_method = nil
      if @inter_class.nil?
        @inter_module[:main].push(new_node)
      else
        @inter_class[:methods][node.children[0].to_s] = new_node
      end
    end
  end

  def on_send(node)
    result = {}
    if node.children[0].nil?
      label = node.children[1].to_s
      if PATTERN_STDLIB.key?(label.to_sym)
        call_node = PATTERN_STDLIB[label.to_sym].call(node.children[2..-1].map { |it| process(it) })
      else
        call_node = {kind: INTER_CALL, args: [{kind: INTER_VARIABLE, label: label}] + node.children[2..-1].map { |it| process(it) }}
      end
      if @inter_method.nil?
        return
      end
      @inter_method[:code].push(call_node)
    end
  end

  def on_lvar(node)
    {kind: INTER_VARIABLE, label: node.children[0].to_s}
  end

  def on_each(node)
    node.updated(nil, process_all(node))
  end

  def on_begin(node)
    node.updated(nil, process_all(node))
  end
end

def generate_ast(path)
  input = File.read(path)
  ast = Parser::CurrentRuby.parse(input)
  puts "AST #{path}"
  # InterProcessor.new(traces, methods, inter_ast).process(ast)
  InterTranslator.new(ast).process
end


def generate_path(path, methods, traces, inter_traces)
  input = File.read(path)
  ast = Parser::CurrentRuby.parse(input)
  inter_traces[path] = generate_inter_traces(traces, methods, ast)
end

OPERATORS = Set.new [:+, :-, :*, :/, :==]
NORMAL = Set.new [:RubyIf, :RubyPair]

def compile_child child
  if child[:kind] == :RubySend
    m = if child[:children][0][:kind] == :Nil
      arg_index = 1
      if !child[:typ].nil? || !child[:children][2].nil?
        {kind: :Call, children: child[:children][1 .. -1].map { |l| compile_child l }, typ: child[:typ]}
      else
        arg_index = -1
        child[:children][1]
      end
    else
      m = if !child[:typ].nil? || !child[:children][2].nil?
        arg_index = 2
        {kind: :Send, children: child[:children].map { |l| compile_child l }, typ: child[:typ]}
      else
        arg_index = -1
        {kind: :Attribute, children: child[:children].map { |l| compile_child l }, typ: child[:typ]}
      end
      if m[:children][1][:kind] == :Variable
        m[:children][1] = {kind: :String, text: m[:children][1][:label]}
      end
      if m[:kind] == :Send && OPERATORS.include?(m[:children][1][:text].to_sym)
        arg_index = -1
        op = m[:children][1]
        op[:kind] = :Operator
        op[:label] = op[:text]
        m = {kind: :BinOp, children: [op, m[:children][0], m[:children][2]], typ: m[:typ]}
      end
      m
    end
    if arg_index != -1
      new_args = []

      m[:children][arg_index .. -1].each do |arg|
        if arg[:kind] == :Table
          new_args += arg[:children]
        else
          new_args.push(arg)
        end
      end

      m[:children] = m[:children][0 .. arg_index - 1] + new_args
    end
    m.tap { |t| t[:line] = child[:line]; t[:column] = child[:column] }
  elsif child.key?(:children)
    res = child
    res[:children] = child[:children].map { |it| compile_child it }
    if NORMAL.include?(res[:kind])
      res[:kind] = res[:kind].to_s[4 .. -1].to_sym
    end
    res
  elsif child.key?(:code)
    res = child
    res[:code] = child[:code].map { |it| compile_child it }
    res
  else
    # p child
    child
  end
end

def compile traces
  $types_no_definition = Hash[$inter_types.map { |k, v| [k, v] }]
  traces.each do |path, file|
    file[:main] = file[:main].map do |child|
      compile_child(child)
    end

    file[:classes] = file[:classes].map do |klass|
      parent = klass[:children][1]
      if parent.nil?
        parent = {kind: :Nil}
      elsif parent[:kind] == :RubyConst
        parent = {kind: :Simple, label: parent[:label]}
      end
      mercy = klass[:children][2]
      if mercy[:kind] == :RubyBegin
        mercy = mercy[:children]
      else
        mercy = klass[:children][2 .. -1]
      end
      if mercy.length == 1 && mercy[0][:kind] == :Code
        children = mercy[0][:children]
        mercy = mercy[0][:children].select { |n| n[:kind] == :NodeMethod }
        b = children.select { |n| n[:kind] != :NodeMethod }
        file[:main] += b.map { |it| compile_child(it) }
      end
      p klass[:children][0][:label]
      $inter_types[klass[:children][0][:label]][:base] = parent
      $types_no_definition.delete klass[:children][0][:label]
      
      {kind: :Class,
       label: klass[:children][0][:label],
       methods: mercy.map { |met| {label: met[:label][:label], node: compile_child(met)} },
       fields: [],
       typ: $inter_types[klass[:children][0][:label]]}
    end
  end
  $inter_traces['%types'] = $types_no_definition
end

def generate
  compile $inter_traces

  File.write($target_folder + "/lang_traces.json", JSON.pretty_generate($inter_traces))
end

$t.enable
$t2.enable

# begin
#   Kernel.load ARGV.first
# rescue => e
#   puts e
#   raise e
# end

at_exit do
  $t.disable
  $t2.disable
  generate
end

