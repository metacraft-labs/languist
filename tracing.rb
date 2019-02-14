require 'json'
require 'parser/current'
require 'ast'

$trace = []
$lines = []
$inter_traces = {}

def trace_calls(data)
  new_trace = [data.path, data.lineno, data.defined_class, data.method_id, data.binding.local_variables.map { |a| [a, data.binding.local_variable_get(a).class]}, :input, nil]
  $trace.push new_trace
end

t = TracePoint.new(:call, :c_call) do |tp|
  if tp.defined_class != TracePoint && tp.defined_class != Kernel && tp.method_id != :disable && tp.method_id != :inherited && tp.defined_class != Class && tp.method_id != :method_added
    t4.disable
    t.disable
    t2.disable
    t3.disable
    if $inter_traces[data.path].nil?
      $inter_traces[data.path] = generate_ast(data.path)
    end
    trace_calls(tp)
    t.enable
    t2.enable
    t3.enable
    t4.enable
  end
end

t2 = TracePoint.new(:return) do |tp|
  $trace.push [tp.path, tp.lineno, tp.defined_class, tp.method_id, [:result, tp.return_value.class], :return, nil]
end

t3 = TracePoint.new(:raise) do |tp|
  exception = tp.raised_exception
  $trace[-1][-1] = exception
end

t4 = TracePoint.new(:line) do |tp|
  t4.disable
  t.disable
  t2.disable
  t3.disable
  if $inter_traces[data.path].nil?
    $inter_traces[data.path] = generate_ast(data.path)
  end
  t.enable
  t2.enable
  t3.enable
  t4.enable
  # if $inter_traces[data.path][:lines].key?(tp.lineno)
  #   $inter_traces[data.path][:lines][tp.lineno].each do |trace|
  #     node.visit do |label|
  #       #label
  #       # just follow calls
  #       # and then just fill those instead of all
        # after this do local inference

  $lines.push([tp.path, tp.lineno])
end


class Type
  attr_reader :kind, :args, :return_type, :label
end

def load_type(arg)
  if arg.nil?
    return {kind: :nil}
  end
  if arg[1] == Integer
    {kind: :int}
  elsif arg[1] == NilClass
    {kind: :nil}
  elsif arg[1] == String
    {kind: :string}
  else
    {kind: :simple, label: arg[1].name}
  end
end

def load_method(args, return_type)
  {kind: :method, args: args.map { |arg| load_type(arg) }, return_type: load_type(return_type)}
end


def union(left, right)
  case left[:kind]
  when :method
    same = left[:args] == right[:args] and left[:return_type] == right[:return_type]
    if same
      [left, true]
    else
      [{kind: :method_overload, overloads: [left, right]}, false]
    end
  when :method_overload
    p left
  when :simple
    if left[:label] == right[:label]
      [left, true]
    else
      [{kind: union, variants: [left, right]}, true]
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

PATTERN_STDLIB = {puts: -> a { {kind: INTER_CALL, children: [{kind: INTER_VARIABLE, label: "echo"}] + a , typ: {kind: :nil} } } }

class InterTranslator
  def initialize(ast)
    @ast = ast
  end

  def process
    res = {imports: [], main: [], classes: [], lines: {}}
    @inter_ast = res
    @ast.children.each do |it|
      if it.kind == :class
        res[:classes].push(process_node it)
      elsif it.kind == :send && it.children[0].nil? && it.children[1] == :require
        res[:imports].push(it.children[2])
      else
        res[:main].push(process_node it)
      end
    end
  end

  def process_node(node)
    if node.class == AST
      value = {kind: KINDS[node.kind], children: node.children.map { |it| process_node it }, typ: nil}
      if !@inter_ast[:lines].key?(node.lineno)
        @inter_ast[:lines][node.lineno] = []
      end
      @inter_ast[:lines][node.lineno].push(value)
      value
    elsif node.class == Integer
      p node
    else
      p node
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
      p node.children[2]
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

def generate(traces, paths)
  inter_traces = {}
  paths.each do |path, methods|
    generate_path(path, methods, traces, inter_traces)
  end
  p inter_traces
  File.write("lang_traces.json", JSON.dump(inter_traces))
end

t.enable
t2.enable
t3.enable
t4.enable

begin
  Kernel.load ARGV.first
rescue
end

at_exit do
  t4.disable
  t.disable
  t2.disable
  t3.disable
  traces, paths = write($trace)
  generate(traces, paths)
  p $trace
end

