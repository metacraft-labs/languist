import os
import sys
import ast_nodes
from colorama import init, Fore, Back, Style

from type_system import (
    PyAtom,
    PyFunction,
    PyFunctionOverloads,
    PyGeneric,
    PyObject,
    PyTuple,
    Variable,
    pyunify,
    PY_NONE,
    PY_LIST,
    PY_DICT,
    KNOWN,
)

MY = 'c'

IGNORED_FILES = {
    '<frozen importlib._bootstrap>',
    '<frozen importlib._bootstrap_external>',
    '__builtin__'
}

IGNORED_FUNCTIONS = {
    'write'
}

PROJECT_DIR = ''
PACKAGE = ''

loaded_modules = {}
env = {}
info = {}
classes = {}

init()

DEBUG_LOG = False

if DEBUG_LOG:
    IGNORED_FUNCTIONS.add('log')


def warn(*a):
    if DEBUG_LOG:
        sys.stdout.write(Fore.YELLOW + 'WARN:')
        for arg in a:
            sys.stdout.write(str(arg))
        sys.stdout.write(Fore.RESET + '\n')


def success(*a):
    if DEBUG_LOG:
        sys.stdout.write(Fore.GREEN)
        for arg in a:
            sys.stdout.write(str(arg))
        sys.stdout.write(Fore.RESET + '\n')


def log(*a):
    if DEBUG_LOG:
        for arg in a:
            sys.stdout.write(str(arg))
        sys.stdout.write('\n')


def load_namespace(filename):
    '''
    generates namespace currently based on directories
    # TODO check __init__.py
    '''
    if not filename.startswith(PROJECT_DIR) or filename[-3:] != '.py':
        return ''
    else:
        tokens = filename[len(PROJECT_DIR):].split('/')
        return PACKAGE + '.'.join(tokens)[:-3]


def profile_calls(frame, event, arg):
    co = frame.f_code
    # print("PROF ", co.co_filename, co.co_firstlineno, event)


def load_module_ast(filename):
    known_module = loaded_modules.get(filename)
    if known_module is None:
        if valid_module(filename):
            result = {'ast': ast_nodes.nodes_from_file(filename)}
            loaded_modules[filename] = result
            return result
        else:
            loaded_modules[filename] = False
            return
    else:
        return known_module


def locate_call_node(ast, line, fn_name):
    if ast is None or ast is False:
        return

    calls = ast["ast"]["nodes_by_line"].get(line)
    if calls is None:
        return

    for call in calls:
        label = call["children"][0].get("label")
        if label == fn_name:
            return call


def trace_calls(frame, event, arg):
    '''
    called on runtime event
    '''
    if event != 'call' and event != 'return':
        return
    co = frame.f_code
    func_name = co.co_name
    func_filename = co.co_filename

    # print(func_filename, func_name, PROJECT_DIR)
    if func_filename[:4] == "/usr" or \
            func_filename in IGNORED_FILES or \
            func_name in IGNORED_FUNCTIONS or \
            not func_filename.startswith(PROJECT_DIR) or \
            func_filename.endswith(MY):
        return

    known_module = loaded_modules.get(func_filename)
    if known_module is None:
        if valid_module(func_filename):
            loaded_modules[func_filename] = {'ast': ast_nodes.nodes_from_file(func_filename)}
        else:
            loaded_modules[func_filename] = False
            return

    log("TRACE", func_filename, func_name, co.co_firstlineno, event)

    return_value = arg if event == 'return' else None
    locals = frame.f_locals
    namespace = load_namespace(func_filename)

    if 'self' in locals:
        class_name = type(locals['self']).__name__
    else:
        class_name = ''

    name = '%s%s%s#%s' % (namespace, '.' if class_name else '', class_name, func_name)

    if return_value is not None:
        value_type = check_value(return_value)
        if name not in env:
            env[name] = PyFunction(func_name, [], [], value_type)
        else:
            env[name].return_type = value_type
    else:
        caller = frame.f_back
        if caller is not None:
            caller_ast = load_module_ast(caller.f_code.co_filename)
            call_node = locate_call_node(caller_ast, caller.f_lineno, func_name)
            if call_node is not None:
                success("RESOLVED", name, call_node)
                call_node["resolved"] = name

        # XXX: debugging code?
        # function = frame.f_globals.get(func_name)
        # if function is None:
        #    function = frame.f_locals.get(func_name)
        args = []
        variables = []
        return_type = PY_NONE
        for z, var in enumerate(frame.f_code.co_varnames + tuple(frame.f_locals.keys())):
            id = class_name + '#' + func_name + ':' + var
            info[id] = ['instance', 'class', func_name, var]
            value = locals.get(var)
            if value is not None:
                # print(var, type(value).__name__, type(value), getattr(value, '__dict__', {}))
                check_object(value, type(value), env)
                if frame.f_code.co_argcount > z:
                    args.append(Variable(var, check_value(value)))
                else:
                    variables.append(Variable(var, check_value(value)))
            else:
                warn("missing variable: " + var)

        info[func_name] = ['instance', 'class', func_name, func_name]
        save_function(name, PyFunction(func_name, args, variables, return_type), env)

    return trace_calls


def start_trace(mainpyfile):
    global PROJECT_DIR
    global PACKAGE
    env.clear()
    info.clear()
    sys.settrace(trace_calls)
    PROJECT_DIR = os.path.dirname(os.path.abspath(mainpyfile))
    PACKAGE = PROJECT_DIR.rpartition('/')[2]
    # XXX: This produces a more detailed output, even thought the docs say it should
    # be the other way arround. It seems that the difference is that profiling covers
    # more of the built-in functions and modules.
    # sys.setprofile(profile_calls)


def finish_trace():
    global loaded_modules
    loaded_modules = {key: value for key, value in loaded_modules.items() if value != False}
    types = {}
    for key, value in env.items():
        if isinstance(value, PyObject) and value.base is not None:
            if value.base in classes:
                value.base = classes[value.base]
                value.base.inherited = True
                fields = list(value.fields.keys())
                for field in fields:
                    base = value.base
                    while base is not None and isinstance(base, PyObject):
                        if field in base.fields:
                            del value.fields[field]
                            break
                        base = base.base
            else:
                value.base = PyAtom(value.base.__name__)

    loaded_modules['@types'] = {key: value.as_json() for key, value in env.items()}
    loaded_modules['@types']['@path'] = sys.path
    loaded_modules['@projectDir'] = PROJECT_DIR
    result = loaded_modules
    env.clear()
    info.clear()
    loaded_modules = {}
    return result


def save_function(name, type, env):
    existing = env.get(name)
    # print(existing, type)
    if existing is None:
        env[name] = type
    elif existing != type:
        if isinstance(existing, PyFunction):
            env[name] = PyFunctionOverloads(type.label, [existing, type])
        elif isinstance(existing, PyFunctionOverloads):
            for overload in existing.overloads:
                if type == overload:
                    return
            existing.overloads.append(type)


def check(id, value, env):
    existing = env.get(id)
    value_type = check_value(value)
    if existing is None:
        env[id] = value_type
    elif existing != value_type:
        env[id] = pyunify(id, value_type)


def check_value(value):
    return check_type(value, type(value), type(value).__name__)


def check_type(value, typ, name):
    kind = KNOWN.get(typ)
    if kind is None:
        return PyAtom(name)
    elif isinstance(kind, PyGeneric):
        if kind.klass == 'list':
            if not value:
                return kind
            else:
                return kind.gen([pyunify(*[check_value(element) for element in value])])
        else:
            if not value:
                return kind
            else:
                return kind.gen([
                    pyunify(*[check_value(key) for key in value.keys()]),
                    pyunify(*[check_value(val) for val in value.values()])
                ])
    elif kind == PyTuple:
        if len(value) > 8:  # probably a list, python people for some reason abuse tuples
            kind = PY_LIST
            return kind.gen([pyunify(*[check_value(element) for element in value])])
        else:
            return PyTuple([check_type(element, type(element), '') for element in value])
    else:
        return kind


def check_object(value, typ, env):
    # print(type(value))
    has = False
    # try:
    try:
        value.__dict__
        has = True
        # has = hasattr(value, '__dict__')
    # except ImportError as e:
    #     raise e
    except AttributeError as e:
        pass
    if typ not in KNOWN and has:
        name = typ.__name__
        if typ.__module__:
            module = sys.modules.get(typ.__module__)
            if module is None:
                warn("no module in sys", typ.__module__)
            else:
                path = getattr(module, "__file__", None)
                if path is None:
                    warn("module without a path", typ.__module__)
                else:
                    name = '%s.%s' % (load_namespace(path), name)
        if name not in env:
            env[name] = PyObject(typ.__name__, None, {})
            if hasattr(typ, '__mro__') and len(typ.__mro__) > 2:  # python3
                env[name].base = typ.__mro__[1]
        if not isinstance(value.__dict__, dict):
            return
        for label, field in value.__dict__.items():
            field_type = check_value(field)
            if label not in env[name].fields:
                env[name].fields[label] = field_type
            elif env[name].fields[label] != field_type:
                env[name].fields[label] = pyunify(env[name].fields[label], field_type)
        classes[typ] = env[name]


def valid_module(path):
    # TODO better check
    return os.path.exists(path) and 'python-deduckt' not in path
