"""Node transformers for manipulating the abstract syntax tree

Transformers for exploding functions, rewriting syntax, and adding annotations exist currently.

"""

import ast
import doctest

import astor


class FunctionExploder(ast.NodeTransformer):
    """Exposes the body of a function to the next scope up

    Transforms:

        def foo(a, b, c):
            '''Consise description

            Longer description is included under the concise description

            >>> a = 1
            >>> b = 2
            >>> c = 3

            '''
            z = a + b + c
            return z

    into something like

        m: # Foo

        m: Consise description

        m: Longer description is included under the concise description

        c: a = 1
        c: b = 2
        c: c = 3
        c: z = a + b + c

    TODO: Handle `return`s better.

    """
    def visit_FunctionDef(self, func):
        [docstring], func.body = func.body[:1], func.body[1:]

        # docstring
        parser = doctest.DocTestParser()
        results = parser.parse(docstring.value.s)
        docstring_prefix, docstring_examples = results[0], [result for result in results if isinstance(result, doctest.Example)]
        assign_exprs = [example.source.strip() for example in docstring_examples]

        # filter returns
        func.body = [stmt for stmt in func.body if not isinstance(stmt, ast.Return)]

        # augment body with docstring
        body = []
        body.append(
            Annotator.make_annotation(
                buffer=func.name,
                content=' '.join(substring.capitalize() for substring in func.name.split('_')),
                cell_type='1'
            )
        )
        body.append(Annotator.make_annotation(buffer=func.name, content=docstring_prefix, cell_type='markdown'))
        body.append(Annotator.make_annotation(buffer=func.name, content='Example Input', cell_type='1'))
        for assign_expr in assign_exprs:
            tree = ast.parse(assign_expr)
            body.append(tree.body[0])
        body.append(Annotator.make_annotation(buffer=func.name, content='Body of Function', cell_type='1'))
        for stmt in func.body:
            body.append(stmt)

        return ast.Module(body=body)

class SyntaxRewriter(ast.NodeTransformer):
    """Performs pure syntax rewrites

    Currently the only syntax rewrite are for loops to while loops. Future
    rewrites include context managers and decorators.

    """
    def visit_For(self, forr):
        """
        for i in iterable:
            <body>

        becomes

        p = iter(iterable)
        while True:
            try:
                i = next(p)
            except StopIteration:
                break
            <body>

        TODO: Choose a random legal variable name to replace hard-coded `p`.

        """
        # p = iter(iterable)
        assign_iter = ast.Assign(
            targets=[ast.Name(id='p', ctx=ast.Store())],
            value=ast.Call(
                func=ast.Name(id='iter', ctx=ast.Load()),
                args=[forr.iter],
                keywords=[]
            )
        )

        # i = next(iter(iterable))
        assign_next = ast.Assign(
            targets=[forr.target],
            value=ast.Call(
                func=ast.Name(id='next', ctx=ast.Load()),
                args=[ast.Name(id='p', ctx=ast.Load())],
                keywords=[]
            )
        )

        # try:
        #     p = iter(iterable)
        # except:
        #     break
        try_node = ast.Try(
            body=[assign_next],
            handlers=[ast.ExceptHandler(type=ast.Name(id='StopIteration', ctx=ast.Load()), name=None, body=[ast.Break()])],
            orelse=[],
            finalbody=[]
        )

        # while True:
        #     try:
        #         p = iter(iterable)
        #     except:
        #        break
        while_node = ast.While(
            test=ast.NameConstant(value=True),
            body=[try_node] + forr.body,
            orelse=[]
        )

        return ast.Module(body=[assign_iter, while_node])

class Annotator(ast.NodeTransformer):
    """Annotates code with commands to create jupyter notebook cells"""
    context_nodes = [ast.If, ast.While, ast.Try, ast.Assign]

    @staticmethod
    def make_annotation(node=None, buffer='outside', content=None, cell_type='code'):
        """Return a ast.Expr that looks like

        __cell__('make-code-cell-and-eval', [content, buffer, cell_type])

        """
        content = astor.to_source(node).strip() if node else content
        call = ast.Call(
            func=ast.Name(id='__cell__', ctx=ast.Load()),
            args=[
                ast.Str(s=content),
                ast.Str(s=f'context={buffer}'),
                ast.Str(s=cell_type)
            ],
            keywords=[]
        )
        return ast.Expr(call)

    def __init__(self, buffer):
        super(__class__, self).__init__()
        self.buffer = buffer

    def _annotate_nodes(self, nodes):
        """Make annotation on the nodes.

        If the node has a context then don't annotate it normally.
        Rather recursively call `visit()` on it.

        """
        body = []
        for node in nodes:
            if any(isinstance(node, node_type) for node_type in Annotator.context_nodes):
                node = self.visit(node)
                body.append(node)
            else:
                body.append(node)
                body.append(Annotator.make_annotation(node, buffer=self.buffer))
        return body

    def visit_If(self, iff):
        return ast.copy_location(
            ast.Module(body=[
                Annotator.make_annotation(buffer=self.buffer, content=f'if {astor.to_source(iff.test).strip()} ...', cell_type='2'),
                Annotator.make_annotation(iff.test, buffer=self.buffer),
                ast.If(
                    test=iff.test,
                    body=self._annotate_nodes(iff.body),
                    orelse=self._annotate_nodes(iff.orelse)
                )
            ]),
            iff
        )

    def visit_While(self, whilst):
        return ast.copy_location(
            ast.Module(body=[
                Annotator.make_annotation(buffer=self.buffer, content=f'while {astor.to_source(whilst.test).strip()} ...', cell_type='2'),
                Annotator.make_annotation(whilst.test, buffer=self.buffer),
                ast.While(
                    test=whilst.test,
                    body=self._annotate_nodes(whilst.body),
                    orelse=self._annotate_nodes(whilst.orelse),    
                )
            ]),
            whilst
        )

    def visit_Try(self, try_):
        handlers = []
        for handler in try_.handlers:
            handlers.append(
                ast.ExceptHandler(
                    type=handler.type,
                    name=None,
                    body=self._annotate_nodes(handler.body)
                )
            )
        return ast.copy_location(
            ast.Try(
                body=self._annotate_nodes(try_.body),
                handlers=handlers,
                orelse=self._annotate_nodes(try_.orelse),
                finalbody=self._annotate_nodes(try_.finalbody)
            ),
            try_
        )

    def visit_Assign(self, assign):
        """Append the targets to the assign code string

        Do the same thing as `generic_visit()` otherwise.

        """
        assign_content, targets_content = astor.to_source(assign), astor.to_source(assign.targets[0])
        content = assign_content + targets_content.strip()
        annotation = Annotator.make_annotation(buffer=self.buffer, content=content)
        return ast.copy_location(ast.Module(body=[assign, annotation]), assign)

    def visit_Call(self, call):
        """Skip annotations if they are already here

        They would get here if FunctionExploder() was called already.

        """
        return self.generic_visit(ast.Expr(call)) if not call.func.id == '__cell__' else ast.Expr(call)

    def visit_Expr(self, expr):
        """Visit the value of Exprs

        Some nodes are represented as a `ast.Expr` as opposed to, say,
        ast.Call. So get the value of the `ast.Expr` so the appropriate node
        transformer gets called.

        """
        return self.visit(expr.value)

    def generic_visit(self, node):
        """Catch-all for visitors that have not been defined."""
        annotation = Annotator.make_annotation(node, buffer=self.buffer)
        return ast.copy_location(ast.Module(body=[node, annotation]), node)

if __name__ == '__main__':
    code = '''

    def foo(a):
        """This is a docstring

        >>> a = 7

        """
        for i in range(a):
            print(i)

    '''
    tree = ast.parse(code)
    tree = FunctionExploder().visit(tree)
    code = astor.to_source(tree)
    print(code)

    tree = ast.parse(code)
    tree = SyntaxRewriter().visit(tree)
    code = astor.to_source(tree)
    print(code)

    tree = ast.parse(code)
    tree.body = [Annotator(buffer='foo').visit(node) for node in tree.body]
    code = astor.to_source(tree)
    print(code)

    code = '''

    if foo in bar:
        width, height = scene_image.size
        for i, obj in enumerate(mod_vec_payload['objects']):
            print(1)
            print(2)

    # Cropping and processing the object patches from the scene image
    object_arrays, object_imgs = [], []
    for i, obj in tqdm(enumerate(mod_vec_payload['objects'])):
        print(3)
        print(4)

    with graph.as_default():
        eprint('GOT TF GRAPH AND VECTORIZING')
        all_object_vectors = predictF2V(xception_ftr_xtrct, object_arrays)

    '''
    tree = ast.parse(code)
    tree = FunctionExploder().visit(tree)
    code = astor.to_source(tree)
    print(code)
    tree = ast.parse(code)
    tree.body = [SyntaxRewriter().visit(node) for node in tree.body]
    code = astor.to_source(tree)
    print(code)
    tree = ast.parse(code)
    tree.body = [Annotator().visit(node) for node in tree.body]
    code = astor.to_source(tree)
    print(code)
