import ast
import astor
from epc.server import EPCServer

server = EPCServer(('localhost', 0))

# if not active_funcname == 'N/A':
#     funcs = [stmt for stmt in tree.body if isinstance(stmt, ast.FunctionDef)]
#     tree = [func for func in funcs if func.name == active_funcname][0]

def make_annotation(expr, ws):
    """Annotate stmt

    Tag a function stmt with its corresponding emacs buffer. So its text goes
    there. If a statement is not a function then tag it with the "outside" buffer.

    """
    elisp_func, emacs_buffer = 'make-code-cell-and-eval', f'context={ws}'
    annotation = f'epc_client.call_sync("""{elisp_func}""", ["""{expr}""", """{emacs_buffer}"""])'
    return annotation

def expand_function(stmt):
    """Expands a function into its body statements

    Expands every other statement into a singleton.

    """
    stmts = cells = None
    if isinstance(stmt, ast.FunctionDef): # unpack functions
        stmts, cells = stmt.body, [stmt.name]*len(stmt.body)
    else:
        stmts, cells = [stmt], ['outside']
    return zip(stmts, cells)

def expand_assign(stmt, ws):
    """Expands a assign statement into its assign plus the identifier

    Expands every other statement into a singleton.

    """
    stmts = cells = None
    if isinstance(stmt, ast.Assign):
        stmts, cells = [stmt, stmt.targets[0]], [ws, ws]
    else:
        stmts, cells = [stmt], [ws]
    return zip(stmts, cells)

@server.register_function
def annotate(*code):
    """Annotate code with code to make and eval cells"""
    code, active_funcname = code[0], code[1]
    tree = ast.parse(code)
    if not active_funcname == 'N/A':
        tree.body = [stmt for stmt in tree.body if isinstance(stmt, ast.FunctionDef) and stmt.name == active_funcname]

    stmts, wss = zip(*[(s, ws) for stmt in tree.body for s, ws in expand_function(stmt)])
    stmts, wss = zip(*[(s, w) for stmt, ws in zip(stmts, wss) for s, w in expand_assign(stmt, ws)])
    exprs = [astor.to_source(stmt).rstrip() for stmt in stmts]
    annotations = [make_annotation(expr, ws) for expr, ws in zip(exprs, wss)]
    nb_expr = len(exprs)
    commands = [None]*(nb_expr*3)
    commands[0::3], commands[1::3], commands[2::3] = exprs, annotations, ['time.sleep(0.01)']*nb_expr
    new_code = '\n'.join(commands)
    return new_code

server.print_port()
server.serve_forever()
