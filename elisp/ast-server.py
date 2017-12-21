import ast
import astor
from epc.server import EPCServer
import doctest

server = EPCServer(('localhost', 0))

# if not active_funcname == 'N/A':
#     funcs = [stmt for stmt in tree.body if isinstance(stmt, ast.FunctionDef)]
#     tree = [func for func in funcs if func.name == active_funcname][0]

def expand_function(stmt):
    """Expands an expression in the case of a function or an assignment

    Expands every other statement into a singleton.

    """
    cells = []
    if isinstance(stmt, ast.FunctionDef): # unpack functions
        func = stmt
        [docstring], body = func.body[:1], func.body[1:]

        # docstring
        parser = doctest.DocTestParser()
        results = parser.parse(docstring.value.s)
        docstring_prefix, docstring_examples = results[0], [result for result in results if isinstance(result, doctest.Example)]
        assign_exprs = [example.source.strip() for example in docstring_examples]

        # body
        body = [stmt for stmt in body if not isinstance(stmt, ast.Return)]
        body_exprs = [astor.to_source(stmt).strip() for stmt in body]

        cells.append({
            'content': ' '.join(substring.capitalize() for substring in func.name.split('_')),
            'type': '1',
            'worksheet': func.name
        })
        cells.append({
            'content': '\n'.join(line[4:] for line in docstring_prefix.split('\n')).strip(),
            'type': 'markdown',
            'worksheet': func.name
        })
        cells.append({
            'content': 'Example Input',
            'type': '1',
            'worksheet': func.name
        })
        for assign_expr in assign_exprs:
            cells.append({
                'code': assign_expr,
                'type': 'code',
                'worksheet': func.name
            })
        cells.append({
            'content': 'Body of Function',
            'type': '1',
            'worksheet': func.name
        })
        for body_expr in body_exprs:
            cells.append({
                'code': body_expr,
                'type': 'code',
                'worksheet': func.name
            })
    else:
        cells.append({
            'code': astor.to_source(stmt).strip(),
            'type': 'code',
            'worksheet': 'outside'
        })
    return cells

def expand_assign(cell):
    """Expand assignment content to display their value after them

    Markdown cells have content but no code. Code cells have code but no
    content. Deal with each of them now.

    """
    if not cell['type'] == 'code':
        cell['code'] = str()
        return cell

    cell['content'] = cell['code']
    stmt = ast.parse(cell['code']).body[0]
    if isinstance(stmt, ast.Assign):
        assign = stmt
        target = assign.targets[0]
        target_expr = astor.to_source(target).strip()
        cell['content'] += f'\n{target_expr}'
    return cell

def make_annotation(expr, ws):
    """Annotate stmt

    Tag a function stmt with its corresponding emacs buffer. So its text goes
    there. If a statement is not a function then tag it with the "outside" buffer.

    """
    return annotation

def wrap_content(cell):
    """Wrap content with RPC calls

    This is the content that will be inserted into cells so we have to wrap it!

    """
    emacs_buffer = f'context={cell["worksheet"]}'
    content, cell_type = cell['content'], cell['type']
    elisp_func, args = f'"""make-code-cell-and-eval"""', f'["""{content}""", """{emacs_buffer}""", """{cell_type}"""]'
    annotation = f'epc_client.call_sync({elisp_func}, {args})'
    cell['content'] = annotation
    return cell

@server.register_function
def annotate(*code):
    """Annotate code with code to make and eval cells"""
    code, active_funcname = code[0], code[1]
    tree = ast.parse(code)
    if not active_funcname == 'N/A':
        tree.body = [stmt for stmt in tree.body if isinstance(stmt, ast.FunctionDef) and stmt.name == active_funcname]

    cells = [cell for stmt in tree.body for cell in expand_function(stmt)]
    print(cells)
    cells = [expand_assign(cell) for cell in cells]
    print(cells)
    cells = [wrap_content(cell) for cell in cells]
    print(cells)

    nb_cell = len(cells)
    commands = [None]*(nb_cell*3)
    exprs, annotations = zip(*[(cell['code'], cell['content']) for cell in cells])
    commands[0::3], commands[1::3], commands[2::3] = exprs, annotations, ['time.sleep(0.01)']*nb_cell
    new_code = '\n'.join(commands)
    return new_code

server.print_port()
server.serve_forever()
