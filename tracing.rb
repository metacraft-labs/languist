require 'json'
$trace = []


def trace_calls(data)
  new_trace = [data.path, data.lineno, data.defined_class, data.method_id, data.binding.local_variables.map { |a| [a, data.binding.local_variable_get(a).class]}, :input]
  $trace.push new_trace
end

t = TracePoint.new(:call) do |tp|
  trace_calls(tp)
end

t2 = TracePoint.new(:return) do |tp|
  if tp.method_id != :initialize
    $trace.push [tp.path, tp.lineno, tp.defined_class, tp.method_id, [:result, tp.return_value.class], :return]
  end
end


t.enable
t2.enable
Kernel.load ARGV.first

class Type
  attr_reader :kind, :args, :return_type, :label
end

def load_type(arg)
  if arg.nil?
    return {kind: :nil}
  end
  case arg[1]
  when String
    {kind: :string}
  when Fixnum
    {kind: :int}
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
  end

  File.write "traces.json", JSON.dump(traces)
end

at_exit do
  t.disable
  t2.disable

  write($trace)
  p $trace
end



