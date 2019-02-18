import sys
import ast
import json

LABEL = 'PyLabel'


def merge(a, b):
    c = a.copy()
    c.update(b)
    return c


BIGGEST_INT = 9223372036854775807
SMALLEST_INT = -9223372036854775808


class JsonTranslator:

    def __init__(self):
        self.source = []
        self.line = 0
        self.column = 0
        self.nodes_by_line = {}

    def translate(self, child):
        return (getattr(self, 'translate_%s' % type(child).__name__.lower(), None) or
                self.translate_child)(child)

    def translate_file(self, root_node, source):
        self.source = source.split('\n')
        result = self.translate(root_node)
        result["nodes_by_line"] = self.nodes_by_line
        return result

    def translate_child(self, child):
        line = getattr(child, 'lineno', -1)
        column = getattr(child, 'col_offset', -1)
        if isinstance(child, ast.AST):
            node_type = type(child).__name__
            left = {
                'kind': 'Py%s' % node_type,
                'children': [
                    self.translate(getattr(child, label))
                    for label in child._fields
                    if label not in {'ctx'}
                ],
                'line': line,
                'column': column
            }

            while len(left['children']) == 1 and left['children'][0]['kind'] == 'Sequence':
                left['children'] = left['children'][0]['children']

            if node_type == "Call":
                nodes = self.nodes_by_line.setdefault(line, [])
                nodes.append(left)

            return left
        elif isinstance(child, list):
            return {
                'kind': 'Sequence',
                'children': [self.translate(son) for son in child],
                'line': line,
                'column': column
            }
        elif child is None:
            return {
                'kind': 'PyNone',
                'line': line,
                'column': column
            }
        elif isinstance(child, bytes):
            return {
                'kind': 'PyBytes',
                's': str(child),
                'line': line,
                'column': column
            }
        elif isinstance(child, int):
            if child < SMALLEST_INT or child > BIGGEST_INT:
                return {
                    'kind': 'PyHugeInt',
                    'h': str(child),
                    'line': line,
                    'column': column
                }
            else:
                return {
                    'kind': 'PyInt',
                    'i': child,
                    'line': line,
                    'column': column
                }
        elif isinstance(child, float):
            return {
                'kind': 'PyFloat',
                'f': child,
                'line': line,
                'column': column
            }
        else:
            return {
                'kind': 'PyStr',
                's': str(child),
                'line': line,
                'column': column
            }

    def translate_nameconstant(self, child):
        line = getattr(child, 'lineno', -1)
        column = getattr(child, 'col_offset', -1)
        return {
            'kind': LABEL,
            'label': str(child.value),
            'line': line,
            'column': column
        }

    def translate_name(self, child):
        line = getattr(child, 'lineno', -1)
        column = getattr(child, 'col_offset', -1)
        return {
            'kind': LABEL,
            'label': child.id,
            'line': line,
            'column': column
        }

    def translate_num(self, child):
        line = getattr(child, 'lineno', -1)
        column = getattr(child, 'col_offset', -1)
        if isinstance(child, ast.Num):
            if isinstance(child.n, int):
                if child.n < SMALLEST_INT or child.n > BIGGEST_INT:
                    return {
                        'kind': 'PyHugeInt',
                        'h': str(child.n),
                        'line': line,
                        'column': column
                    }
                else:
                    return {
                        'kind': 'PyInt',
                        'i': child.n,
                        'line': line,
                        'column': column
                    }
            else:
                return {
                    'kind': 'PyFloat',
                    'f': child.n,
                    'line': line,
                    'column': column
                }
        else:
            if isinstance(child, int):
                if child < SMALLEST_INT or child > BIGGEST_INT:
                    return {
                        'kind': 'PyHugeInt',
                        'h': str(child),
                        'line': line,
                        'column': column
                    }
                else:
                    return {
                        'kind': 'PyInt',
                        'i': child,
                        'line': line,
                        'column': column
                    }
            else:
                return {
                    'kind': 'PyFloat',
                    'f': child,
                    'line': line,
                    'column': column
                }

    def translate_str(self, child):
        line = getattr(child, 'lineno', -1)
        column = getattr(child, 'col_offset', -1)
        if isinstance(child, ast.Str):
            return {
                'kind': 'PyStr',
                's': child.s,
                'line': line,
                'column': column
            }
        else:
            return {
                'kind': 'PyStr',
                's': child,
                'line': line,
                'column': column
            }

    def translate_bytes(self, child):
        line = getattr(child, 'lineno', -1)
        column = getattr(child, 'col_offset', -1)
        if isinstance(child, ast.Bytes):
            return {
                'kind': 'PyBytes',
                's': str(child.s)[2:-1],
                'line': line,
                'column': column
            }
        else:
            return {
                'kind': 'PyBytes',
                's': str(child)[2:-1],
                'line': line,
                'column': column
            }


def nodes_from_source(source):
    return JsonTranslator().translate_file(ast.parse(source), source)


def nodes_from_file(filename):
    with open(filename, 'r') as f:
        return nodes_from_source(f.read())


if __name__ == '__main__':
    if '.py' not in sys.argv[1]:
        source_path = '%s.py' % sys.argv[1]
    else:
        source_path = sys.argv[1]

    nodes = nodes_from_file(source_path)
    print(json.dumps(nodes, indent=4))
