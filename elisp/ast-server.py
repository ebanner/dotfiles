import ast
import astor
from epc.server import EPCServer

server = EPCServer(('localhost', 0))

@server.register_function
def annotate(*code):
    """Annotate code with code to make and eval cells"""
    code = code[0]
    tree = ast.parse(code)
    stmts = [astor.to_source(stmt).rstrip() for stmt in tree.body]
    cell_commands = [f'epc_client.call_sync("""make-code-cell-and-eval""", ["""{stmt}"""])' for stmt in stmts]
    nb_stmt = len(tree.body)
    commands = [None]*(nb_stmt*3)
    commands[0::3], commands[1::3], commands[2::3] = stmts, cell_commands, ['time.sleep(0.01)']*nb_stmt
    new_code = '\n'.join(commands)
    return new_code

server.print_port()
server.serve_forever()
