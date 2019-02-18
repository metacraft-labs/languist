import sys
import os
import traceback
import json
from tracing import start_trace, finish_trace


def main():
    if not sys.argv[1:] or sys.argv[1] in ("--help", "-h"):
        print("usage: python-deduckt scriptfile [arg] ...")
        sys.exit(2)

    # Get script filename
    mainpyfile = os.path.abspath(sys.argv[1])
    if not os.path.exists(mainpyfile):
        print('Error:', mainpyfile, 'does not exist')
        sys.exit(1)

    # Hide "python-deduckt" from the argument list
    del sys.argv[0]

    # Replace pdb's dir with script's dir in front of module search path.
    sys.path[0] = os.path.dirname(mainpyfile)

    with open(mainpyfile, "rb") as fp:
        mainpycode = compile(fp.read(), mainpyfile, "exec")

    # The script has to run in __main__ namespace (or imports from
    # __main__ will break).
    #
    # So we clear up the __main__ and set several special variables
    # (this gets rid of pdb's globals and cleans old variables on restarts).
    import __main__
    __main__.__dict__.clear()
    __main__.__dict__.update({"__name__": "__main__",
                              "__file__": mainpyfile,
                              "__builtins__": __builtins__})

    globals = __main__.__dict__
    locals = globals

    try:
        start_trace(mainpyfile)
        exec(mainpycode, globals, locals)
        sys.settrace(None)

        trace_data = finish_trace()

        with open('python-deduckt.json', 'w') as h:
            h.write(json.dumps(trace_data, indent=4))
    except:
        sys.settrace(None)
        traceback.print_exc()
        sys.exit(3)


if __name__ == '__main__':
    main()
