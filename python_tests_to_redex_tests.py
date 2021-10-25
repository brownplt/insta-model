import ast
from os import error
from typing import List, Union


class symbol(str):
    pass


class string(str):
    pass


def expr_to_type(expr: ast.expr):
    if expr is None:
        return symbol('dynamic')
    elif isinstance(expr, ast.Name):
        return symbol(str(expr.id))
    elif isinstance(expr, ast.Subscript):
        return [
            symbol("subscript"),
            symbol(str(expr.value.id)),
            expr_to_type(expr.slice)]
    elif isinstance(expr, ast.Constant) and expr.value is None:
        return symbol('None')
    elif isinstance(expr, ast.Index):
        return expr_to_type(expr.value)
    elif isinstance(expr, ast.Tuple):
        return [symbol('tuple-syntax')] + [expr_to_type(e) for e in expr.elts]
    elif isinstance(expr, ast.BinOp) and isinstance(expr.op, ast.BitOr):
        return [symbol('or-syntax'), expr_to_type(expr.left), expr_to_type(expr.right)]
    else:
        raise Exception("Can't deal with {}".format(expr))


def stmt_to_class_member(stmt: ast.stmt):
    if isinstance(stmt, ast.AnnAssign):
        assert stmt.value is None
        return [
            symbol('field'),
            string(str(stmt.target.id)),
            expr_to_type(stmt.annotation)]
    elif isinstance(stmt, ast.FunctionDef):
        inputs = [[symbol(str(a.arg)), expr_to_type(a.annotation)]
                  for a in stmt.args.args[1:]]
        output_type = expr_to_type(stmt.returns)
        return [
            symbol('method'),
            string(str(stmt.name)),
            symbol(str(stmt.args.args[0].arg)),
            inputs,
            output_type
        ] + [
            ast_to_sexp(s) for s in stmt.body
        ]
    else:
        raise Exception("Can't deal with {}".format(stmt))


def ast_to_sexp(node):
    if isinstance(node, ast.Module):
        return [ast_to_sexp(e) for e in node.body]
    elif isinstance(node, ast.ClassDef):
        if len(node.bases) == 0:
            bases = [symbol('object')]
        else:
            bases = [ast_to_sexp(b) for b in node.bases]
        return [
            symbol('class'),
            symbol(node.name),
            bases
        ] + [
            stmt_to_class_member(s) for s in node.body if not isinstance(s, ast.Pass)
        ]
    elif isinstance(node, ast.Return):
        if node.value is None:
            return [symbol('return'), symbol('None')]
        else:
            return [symbol('return'), ast_to_sexp(node.value)]
    elif isinstance(node, ast.AnnAssign):
        if node.value is None:
            return [
                symbol('claim'),
                symbol(str(node.target.id)),
                expr_to_type(node.annotation)
            ]
        else:
            return [
                symbol('define/assign'),
                symbol(str(node.target.id)),
                expr_to_type(node.annotation),
                ast_to_sexp(node.value)
            ]
    elif isinstance(node, ast.Assign):
        assert len(node.targets) == 1
        return [
            symbol('define/assign'),
            ast_to_sexp(node.targets[0]),
            symbol('dynamic'),
            ast_to_sexp(node.value)
        ]
    elif isinstance(node, ast.Pass):
        return symbol('pass')
    elif isinstance(node, ast.Expr):
        return [
            symbol('expr'),
            ast_to_sexp(node.value)
        ]
    elif isinstance(node, ast.Name):
        return symbol(str(node.id))
    elif isinstance(node, ast.Constant):
        if isinstance(node.value, str):
            return string(node.value)
        elif node.value is None:
            return symbol('None')
        else:
            return node.value
    elif isinstance(node, ast.Call):
        assert node.keywords == []
        return [ast_to_sexp(node.func)] + [ast_to_sexp(a) for a in node.args]
    elif isinstance(node, ast.ImportFrom):
        return [symbol('import-from'), string(node.module), [string(a.name) for a in node.names]]
    elif isinstance(node, ast.Dict):
        return [
            symbol('dict-syntax')
        ] + [
            [ast_to_sexp(k), ast_to_sexp(v)]
            for k, v in zip(node.keys, node.values)
        ]
    elif isinstance(node, ast.Subscript):
        return [
            symbol("subscript"),
            symbol(str(node.value.id)),
            ast_to_sexp(node.slice)]
    elif isinstance(node, ast.Constant) and node.value is None:
        return symbol('None')
    elif isinstance(node, ast.Index):
        return ast_to_sexp(node.value)
    elif isinstance(node, ast.Tuple):
        return [symbol('tuple-syntax')] + [ast_to_sexp(e) for e in node.elts]
    elif isinstance(node, ast.Delete):
        assert len(node.targets) == 1
        target = node.targets[0]
        return [
            symbol('delete'),
            ast_to_sexp(target)
        ]

        return [symbol('delete'), ast_to_sexp(node.targets[0])]
    elif isinstance(node, ast.Attribute):
        return [symbol('attribute'), ast_to_sexp(node.value), string(node.attr)]
    elif isinstance(node, ast.BinOp):
        assert isinstance(node.op, ast.Add)
        return [
            [
                symbol('attribute'),
                ast_to_sexp(node.left),
                string('__add__')
            ],
            ast_to_sexp(node.right)]
    elif isinstance(node, ast.FunctionDef):
        return [
            symbol('def'),
            symbol(str(node.name)),
            arguments_to_sexp(node.args),
            expr_to_type(node.returns),
            *[ast_to_sexp(stmt) for stmt in node.body]
        ]
    elif isinstance(node, ast.If):
        return [
            symbol('if'),
            ast_to_sexp(node.test),
            [
                ast_to_sexp(s)
                for s in node.body
            ],
            [
                ast_to_sexp(s)
                for s in node.orelse
            ]
        ]
    elif isinstance(node, ast.BoolOp):
        return [
            ast_to_sexp(node.op)
        ] + [
            ast_to_sexp(v)
            for v in node.values
        ]
    elif isinstance(node, ast.Or):
        return symbol('or')
    elif isinstance(node, ast.Compare):
        everything = False
        left = ast_to_sexp(node.left)
        for (op, right) in zip(node.ops, node.comparators):
            op = ast_to_sexp(op)
            right = ast_to_sexp(right)
            everything = [
                symbol('and'),
                everything,
                [op, left, right]
            ]
            left = right
        return everything
    elif isinstance(node, ast.Is):
        return symbol('is')
    elif isinstance(node, ast.Gt):
        return symbol('>')
    elif isinstance(node, ast.Eq):
        return symbol('==')
    elif isinstance(node, ast.While):
        return [
            symbol('while'),
            ast_to_sexp(node.test),
            list(map(ast_to_sexp, node.body)),
            list(map(ast_to_sexp, node.orelse))
        ]
    elif isinstance(node, ast.Break):
        return symbol('break')
    elif isinstance(node, ast.Continue):
        return symbol('continue')
    assert False, str(node)


def arg_to_sexp(a):
    return [symbol(str(a.arg)), expr_to_type(a.annotation)]


def arguments_to_sexp(args):
    assert args.posonlyargs == []
    assert args.vararg == None
    assert args.kwonlyargs == []
    assert args.kw_defaults == []
    assert args.kwarg == None
    assert args.defaults == []
    return [arg_to_sexp(a) for a in args.args]


def python_file_to_sexp(test_file):
    with open(test_file) as f:
        p = ast.parse(f.read(), type_comments=True)
        return ast_to_sexp(p)


def python_file_to_redex_static_test(test_file):
    print("Working on " + test_file)
    spec = open(test_file).readlines()[1]
    if spec == '# This should pass.\n':
        check = symbol('check-judgment-holds*')
    elif spec == '# This should fail.\n':
        check = symbol('check-not-judgment-holds*')
    else:
        assert False, repr(spec)
    return [
        check,
        [
            symbol('⊢p'),
            python_file_to_sexp(test_file)
        ]
    ]

def python_file_to_redex_dynamic_test(test_file):
    assert False, "NotImplemented"
    print("Working on " + test_file)
    spec = open(test_file).readlines()[1]
    if spec == '# This should pass.\n':
        check = symbol('check-judgment-holds*')
    elif spec == '# This should fail.\n':
        check = symbol('check-not-judgment-holds*')
    else:
        assert False, repr(spec)
    return [
        check,
        [
            symbol('⊢p'),
            python_file_to_sexp(test_file)
        ]
    ]


def sexp_to_str(sexp):
    if isinstance(sexp, list):
        return '(' + ' '.join(map(sexp_to_str, sexp)) + ')'
    elif isinstance(sexp, bool):
        return '#t' if sexp else '#f'
    elif isinstance(sexp, symbol):
        return sexp
    elif isinstance(sexp, string):
        return '"' + repr(sexp)[1:-1] + '"'
    elif isinstance(sexp, int):
        return str(sexp)
    elif isinstance(sexp, float):
        return str(sexp)
    else:
        assert False, "Can't deal with {}".format(repr(sexp))


def main():
    import os
    # list all tests in the conformance_suite directory
    test_files = [os.path.join(d, f) for d, _, files in os.walk(
        'conformance_suite') for f in files if f.endswith('.py')]
    test_files.sort()
    # output redex test
    with open('test-statics.rkt', 'w') as f:
        f.write('\n'.join([
            '#lang racket',
            '(require redex)',
            '(require redex-abbrevs)',
            '(require "model.rkt")',
            '(require "statics.rkt")',
            ''
        ]))
        for test_file in test_files:
            test = python_file_to_redex_static_test(test_file)
            f.write('\n')
            f.write(';; ' + test_file + '\n')
            f.write(sexp_to_str(test))
            f.write('\n')


main()
