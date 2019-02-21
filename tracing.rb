require 'json'
require 'parser/current'
require 'ast'

$trace = []
$lines = []
$inter_traces = {}
$inter_types = {}
$call_lines = []

def trace_calls(data)
  new_trace = [data.path, data.lineno, data.defined_class, data.method_id, data.binding.local_variables.map { |a| [a, data.binding.local_variable_get(a).class]}, :input, nil]
  $call_lines.push(data.lineno)
  if $inter_traces[data.path][:method_lines].key?(data.lineno)
    p data.lineno
    $inter_traces[data.path][:method_lines][data.lineno][:args].each do |arg|
      if arg[:label] == :self
        arg[:typ] = load_type(data.binding.receiver.class)
      else
        arg[:typ] = load_type(data.binding.local_variable_get(arg[:label]).class)
      end
    end
  end
end

t = TracePoint.new(:call, :c_call) do |tp|
  p tp
  # !tp.path.start_with?("/usr/lib/ruby/")
  if tp.path != "tracing.rb" && tp.defined_class != TracePoint && tp.defined_class != Kernel && tp.method_id != :disable && tp.method_id != :inherited && tp.defined_class != Class && tp.method_id != :method_added
    if !$inter_traces.key?(tp.path)
      $inter_traces[tp.path] = generate_ast(tp.path)
    end
    trace_calls(tp)
  end
end

t2 = TracePoint.new(:return, :c_return) do |tp|
  if tp.method_id != :new && tp.method_id != :initialize && caller.length > 1 && tp.path != "tracing.rb" && !tp.path.include?("did_you_mean")
    path, line, *_ = caller[1].split(':')
    line = line.to_i
    path = tp.path
    line = tp.lineno
    typ = load_type(tp.return_value.class)
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
        p method_line
        $inter_traces[tp.path][:method_lines][method_line][:return_type] = typ
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

def load_type(arg)
  if arg.nil?
    return {kind: :Simple, label: "Void"}
  end
  res = if arg == Integer
    {kind: :Simple, label: "Int"}
  elsif arg == NilClass
    {kind: :Simple, label: "Void"}
  elsif arg == String
    {kind: :Simple, label: "String"}
  elsif arg == TrueClass || arg == FalseClass
    {kind: :Simple, label: "Bool"}
  else
    {kind: :Simple, label: arg.name}
  end
  if res[:kind] == :Object
    $inter_types[res[:label]] = res
  end
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

KINDS = {lvasgn: :Assign}

class InterTranslator
  def initialize(ast)
    @ast = ast
  end

  def process
    res = {imports: [], main: [], classes: [], lines: {}, method_lines: {}}
    @inter_ast = res
    @ast.children.each do |it|
      if it.type == :class
        res[:classes].push(process_node it)
      elsif it.type == :send && it.children[0].nil? && it.children[1] == :require
        res[:imports].push(it.children[2].children[0])
      else
        # p it, process_node(it)
        res[:main].push(process_node it)
      end
    end
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
      if node.type == :def
        value = {kind: :NodeMethod, label: {typ: :Variable, label: node.children[0]}, args: [], code: [], typ: nil, return_type: nil}
        value[:args] = [{kind: :Variable, label: :self, typ: nil}] + node.children[1].children.map { |it| process_node it }
        value[:code] = node.children[2 .. -1].map { |it| process_node it }
        # value[:args] = args
        # value[:code] 
        @inter_ast[:method_lines][node.loc.line] = value
        return value.tap { |t| t[:line] = node.loc.line; t[:column] = node.loc.column }
      end
      value = {kind: get_kind(node.type), children: node.children.map { |it| process_node it }, typ: nil}
      if node.type == :send
        if value[:children][0][:kind] == :RubyConst && value[:children][1][:kind] == :Variable && value[:children][1][:label] == :new
          value = {kind: :New, children: [{kind: :Variable, label: value[:children][0][:label]}] + value[:children][2 .. -1]}          
        end
        begin
          if !@inter_ast[:lines].key?(node.loc.line)
            @inter_ast[:lines][node.loc.line] = []
          end
          @inter_ast[:lines][node.loc.line].push(value)
          value
        rescue
          {kind: :Nil}
        end
      else
        value
      end.tap { |t| if !node.nil?; t[:line] = node.loc.line; t[:column] = node.loc.column; end }
    elsif node.class == Integer
      {kind: :Int, i: node, typ: nil}
    elsif node.class == String
      {kind: :String, text: node, typ: nil}
    elsif node.class == Symbol
      {kind: :Variable, label: node, typ: nil}
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

  def process_ivar(node)
    {kind: :Attribute, children: [{kind: :Self}, {kind: :Variable, label: node.children[0]}]}
  end

  def process_lvar(node)
    {kind: :Variable, label: node.children[0]}
  end

  def process_nil(node)
    {kind: :Nil}
  end

  def process_arg(node)
    {kind: :Variable, label: node.children[0]}
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
  p path
  # InterProcessor.new(traces, methods, inter_ast).process(ast)
  InterTranslator.new(ast).process
end

# def generate_inter_traces(traces, methods, ast)
#   inter_ast = {imports: [], main: [], classes: []}
#   InterProcessor.new(traces, methods, inter_ast).process(ast)
#   inter_ast
# end

def generate_path(path, methods, traces, inter_traces)
  input = File.read(path)
  ast = Parser::CurrentRuby.parse(input)
  inter_traces[path] = generate_inter_traces(traces, methods, ast)
end

def compile_child child
  if child[:kind] == :RubySend
    if child[:children][0][:kind] == :Nil
      if !child[:typ].nil? || !child[:children][2].nil?
        {kind: :Call, children: child[:children][1 .. -1].map { |l| compile_child l }, typ: child[:typ]}
      else
        child[:children][1]
      end
    else
      if !child[:typ].nil? || !child[:children][2].nil?
        m = {kind: :Send, children: child[:children].map { |l| compile_child l }, typ: child[:typ]}
        p "send"
        p m[:children]
        if m[:children][1][:kind] == :Variable
          m[:children][1] = {kind: :String, text: m[:children][1][:label]}
        end
        m
      else
        {kind: :Attribute, children: child[:children].map { |l| compile_child l }, typ: child[:typ]}
      end
    end.tap { |t| t[:line] = child[:line]; t[:column] = child[:column] }
  elsif child.key?(:children)
    res = child
    res[:children] = child[:children].map { |it| compile_child it }
    res
  elsif child.key?(:code)
    res = child
    res[:code] = child[:code].map { |it| compile_child it }
    res
  else
    p child
    child
  end
end

def compile traces
  traces.each do |path, file|
    file[:main] = file[:main].map do |child|
      compile_child(child)
    end
    file[:classes] = file[:classes].map do |klass|
      {kind: klass[:kind],
       label: klass[:children][0][:label],
       methods: klass[:children][2 .. -1].map { |met| {label: met[:label][:label], node: compile_child(met)} },
       fields: [],
       typ: $inter_types[klass[:children][0][:label]]}
    end
  end
end

def generate
  compile $inter_traces

  File.write("lang_traces.json", JSON.pretty_generate($inter_traces))
end

t.enable
t2.enable
t3.enable

begin
  Kernel.load ARGV.first
rescue => e
  puts e
  raise e
end

at_exit do
  t.disable
  t2.disable
  t3.disable
  generate
end

