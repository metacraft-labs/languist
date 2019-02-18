import ast
require 'sexp'

module Generator
  def gen(path, source)
    @module = path.split('/')[-1].split('.')[0]
    @node = ast.parse(source)
        print(node)
        for klass in node.body:
            if isinstance(klass, ast.ClassDef):
                name = klass.name
                program = self.gen_klass(name, klass, module)
                break
        else:
            return ''

        return self.q('''
            import %s
            import sys
            from helpers import trace_calls, trace_analyze, trace, trace_return

            %s

            sys.settrace(None)
            print(trace_analyze(trace, trace_return))
        ''' % (module, program))


    def gen_klass(self, entry, klass, module):
        functions = []
        for item in klass.body:
            if isinstance(item, ast.FunctionDef):
                functions.append('t.%s()' % item.name)

        return '''
            t = %s.%s()

            sys.settrace(trace_calls)

            try:
                %s
            except Exception as e:
                pass
            ''' % (module, entry, '\n                '.join(functions))

    def q(self, text):
        print(text)
        return '\n'.join(char[12:] for char in text.split('\n')) + '\n'
