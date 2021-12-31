import ast
from os import error
from typing import List, Union
from sexp import string, symbol as sexp_symbol, str_of_sexp


def symbol(s):
    return sexp_symbol(s.replace('_', '-'))


def expr_to_type(expr: ast.expr):
    if expr is None:
        return symbol('dynamic')
    else:
        return ast_to_sexp(expr)


def ast_to_sexp(node):
    if isinstance(node, ast.Module):
        return [ast_to_sexp(s) for s in node.body]
    elif isinstance(node, ast.ClassDef):
        return [
            symbol('class'),
            string(node.name),
            [
                ast_to_sexp(e) for e in node.bases
            ],
            [
                ast_to_sexp(s) for s in node.body
            ]
        ]

    elif isinstance(node, ast.Return):
        if node.value is None:
            return [symbol('return'), [symbol('con'), symbol('None')]]
        else:
            return [symbol('return'), ast_to_sexp(node.value)]
    elif isinstance(node, ast.AnnAssign):
        if node.value is None:
            return [
                symbol('ann-assign'),
                string(str(node.target.id)),
                expr_to_type(node.annotation)
            ]
        else:
            return [
                symbol('ann-assign'),
                ast_to_sexp(node.target),
                expr_to_type(node.annotation),
                ast_to_sexp(node.value)
            ]
    elif isinstance(node, ast.Assign):
        return [
            symbol('assign'),
            [
                ast_to_sexp(t) for t in node.targets
            ],
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
        return string(str(node.id))
    elif isinstance(node, ast.Constant):
        if isinstance(node.value, str):
            return [
                symbol('con'),
                string(node.value)
            ]
        elif node.value is None:
            return [
                symbol('con'),
                symbol('None')
            ]
        else:
            return [
                symbol('con'),
                node.value
            ]
    elif isinstance(node, ast.Call):
        assert node.keywords == []
        return [
            symbol('call'),
            ast_to_sexp(node.func),
            [ast_to_sexp(a) for a in node.args]
        ]
    elif isinstance(node, ast.ImportFrom):
        return [
            symbol('import-from'),
            string(node.module),
            [
                string(a.name) for a in node.names
            ]
        ]
    elif isinstance(node, ast.Dict):
        return [
            symbol('dict'),
            [
                [ast_to_sexp(k), ast_to_sexp(v)]
                for k, v in zip(node.keys, node.values)
            ]
        ]
    elif isinstance(node, ast.Set):
        return [
            symbol('set'),
            [
                ast_to_sexp(v)
                for v in node.elts
            ]
        ]
    elif isinstance(node, ast.Tuple):
        return [
            symbol('tuple'),
            [
                ast_to_sexp(e) for e in node.elts
            ]
        ]
    elif isinstance(node, ast.Subscript):
        return [
            symbol("subscript"),
            ast_to_sexp(node.value),
            ast_to_sexp(node.slice)]
    elif isinstance(node, ast.Index):
        return ast_to_sexp(node.value)
    elif isinstance(node, ast.Delete):
        assert len(node.targets) == 1
        target = node.targets[0]
        return [
            symbol('delete'),
            ast_to_sexp(target)
        ]
    elif isinstance(node, ast.Attribute):
        return [
            symbol('attribute'),
            ast_to_sexp(node.value),
            string(node.attr)
        ]
    elif isinstance(node, ast.UnaryOp):
        return [
            symbol('unary-op'),
            ast_to_sexp(node.op),
            ast_to_sexp(node.operand)
        ]
    elif isinstance(node, ast.BinOp):
        return [
            symbol('bin-op'),
            ast_to_sexp(node.op),
            ast_to_sexp(node.left),
            ast_to_sexp(node.right)
        ]
    elif isinstance(node, ast.BitOr):
        return symbol('bit-or')
    elif isinstance(node, ast.Mult):
        return symbol('*')
    elif isinstance(node, ast.Add):
        return symbol('+')
    elif isinstance(node, ast.Sub):
        return symbol('-')
    elif isinstance(node, ast.USub):
        return symbol('-')
    elif isinstance(node, ast.FunctionDef):
        return [
            symbol('function-def'),
            string(str(node.name)),
            arguments_to_sexp(node.args),
            expr_to_type(node.returns),
            [
                ast_to_sexp(stmt) for stmt in node.body
            ]
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
            symbol('bool-op'),
            ast_to_sexp(node.op),
            [
                ast_to_sexp(v)
                for v in node.values
            ]
        ]
    elif isinstance(node, ast.Or):
        return symbol('or')
    elif isinstance(node, ast.And):
        return symbol('and')
    elif isinstance(node, ast.Compare):
        return [
            symbol('compare'),
            ast_to_sexp(node.left),
            [
                [
                    ast_to_sexp(op),
                    ast_to_sexp(right)
                ]
                for (op, right) in zip(node.ops, node.comparators)
            ]
        ]
    elif isinstance(node, ast.Is):
        return symbol('is')
    elif isinstance(node, ast.IsNot):
        return symbol('is-not')
    elif isinstance(node, ast.In):
        return symbol('in')
    elif isinstance(node, ast.NotIn):
        return symbol('not-in')
    elif isinstance(node, ast.Gt):
        return symbol('>')
    elif isinstance(node, ast.LtE):
        return symbol('<=')
    elif isinstance(node, ast.Eq):
        return symbol('==')
    elif isinstance(node, ast.Not):
        return symbol('not')
    elif isinstance(node, ast.AugAssign):
        return [
            symbol('aug-assign'),
            ast_to_sexp(node.target),
            ast_to_sexp(node.op),
            ast_to_sexp(node.value)
        ]
    elif isinstance(node, ast.IfExp):
        return [
            symbol('if-exp'),
            ast_to_sexp(node.test),
            ast_to_sexp(node.body),
            ast_to_sexp(node.orelse)
        ]
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
    elif isinstance(node, ast.With):
        return [
            symbol('with'),
            [
                ast_to_sexp(item) for item in node.items
            ],
            [
                ast_to_sexp(stmt) for stmt in node.body
            ]
        ]
    elif isinstance(node, ast.withitem):
        return [
            ast_to_sexp(node.context_expr),
            symbol('as'),
            ast_to_sexp(node.optional_vars)
        ]
    elif isinstance(node, ast.Assert):
        return [
            symbol('assert'),
            ast_to_sexp(node.test)
        ]
    elif isinstance(node, ast.Lambda):
        return [
            symbol('lambda'),
            arguments_to_sexp(node.args),
            ast_to_sexp(node.body)
        ]
    elif isinstance(node, ast.Try):
        return [
            symbol('try-except-else-finally'),
            [ast_to_sexp(s) for s in node.body],
            [ast_to_sexp(h) for h in node.handlers],
            [ast_to_sexp(s) for s in node.orelse],
            [ast_to_sexp(s) for s in node.finalbody]
        ]
    elif isinstance(node, ast.Raise):
        assert node.cause is None
        assert node.exc is not None
        return [
            symbol('raise'),
            ast_to_sexp(node.exc)
        ]
    elif isinstance(node, ast.ExceptHandler):
        if node.name is None:
            name = symbol('None')
        else:
            name = string(node.name)
        return [
            symbol('except-handler'),
            ast_to_sexp(node.type),
            name,
            [ast_to_sexp(s) for s in node.body],
        ]
    elif isinstance (node, ast.List):
        return [
            symbol('list'),
            [ ast_to_sexp(e) for e in node.elts ]
        ]
    elif isinstance(node, ast.ListComp):
        return [
            symbol('list-comp'),
            ast_to_sexp(node.elt),
            [ast_to_sexp(g) for g in node.generators]
        ]
    elif isinstance(node, ast.comprehension):
        return [
            ast_to_sexp(node.target),
            ast_to_sexp(node.iter),
            [ ast_to_sexp(cnd) for cnd in node.ifs ]
        ]
    elif isinstance(node, ast.For):
        return [
            symbol('for'),
            ast_to_sexp(node.target),
            ast_to_sexp(node.iter),
            [ast_to_sexp(s) for s in node.body],
            [ast_to_sexp(s) for s in node.orelse]
        ]
    elif node is None:
        return symbol('None')
    assert False, str(node)


def arg_to_sexp(a):
    return [string(str(a.arg)), expr_to_type(a.annotation)]


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


def parse_python_file(test_file):
    print("Working on " + test_file)
    name = test_file
    lines = open(test_file).readlines()
    prog = python_file_to_sexp(test_file)
    base = 1
    if lines[base] == '# This should fail.\n':
        spec = {'compile': False }
    elif lines[base] == '# This should pass.\n':
        spec = {'compile': True }
        base += 1
        if lines[base] == '# This is an optimization test.\n':
            spec['optimization'] = True
            base += 1
        else:
            spec['optimization'] = False
        if lines[base] == '# This should terminate.\n':
            spec['run'] = True
        elif lines[base] == '# This should error.\n':
            spec['run'] = False
        else:
            spec['run'] = None
    else:
        assert False, repr(lines[base])
    source = open(test_file).readlines()
    return name, spec, prog, source


def python_file_to_redex_grammar_test(spec, prog):
    return [
        symbol('test-match'),
        symbol('SP'),
        symbol('program+'),
        [
            symbol('term'),
            prog
        ]
    ]


def python_file_to_redex_desugar_test(spec, prog):
    return [
        symbol('test-match'),
        symbol('SP-core'),
        symbol('program'),
        [
            symbol('term'),
            [
                symbol('desugar-program'),
                prog
            ]
        ]
    ]


def python_file_to_redex_static_test(spec, prog):
    if spec['compile']:
        check = symbol('check-judgment-holds*')
    else:
        check = symbol('check-not-judgment-holds*')
    return [
        check,
        [
            symbol('⊢p'),
            [
                symbol('desugar-program'),
                prog
            ]
        ]
    ]


def python_file_to_redex_compile_test(spec, prog):
    if spec['compile']:
        return [
            symbol('test-match'),
            symbol('SP-compiled'),
            symbol('program-'),
            [
                symbol('term'),
                [
                    symbol('compile-program'),
                    [
                        symbol('desugar-program'),
                        prog
                    ]
                ]
            ]
        ]
    else:
        return [
            symbol('check-exn'),
            symbol('exn:fail:redex?'),
            [
                symbol('lambda'),
                [],
                [
                    symbol('term'),
                    [
                        symbol('compile-program'),
                        [
                            symbol('desugar-program'),
                            prog
                        ]
                    ]
                ]
            ]
        ]


def python_file_to_redex_dynamic_test(spec, prog):
    if spec['run']:
        check = [
            symbol('terminate'),
        ]
    else:
        check = [
            symbol('error'),
            symbol('any'),
        ]
    return [
        symbol('test-match'),
        symbol('SP-dynamics'),
        check,
        [
            symbol('term'),
            [
                symbol('calc'),
                [
                    symbol('compile-program'),
                    [
                        symbol('desugar-program'),
                        prog
                    ]
                ]
            ]
        ]
    ]


def python_file_to_redex_optimization_test(spec, prog):
    return [
        symbol('test-match'),
        symbol('SP-compiled'),
        symbol('any'),
        [
            symbol('term'),
            [
                symbol('compile-program'),
                [
                    symbol('desugar-program'),
                    prog
                ]
            ]
        ]
    ]


path_to_conformance_suite = 'conformance_suite'
path_to_test_grammar = './test-grammar.rkt'
path_to_test_desugar = './test-desugar.rkt'
path_to_test_statics = './test-statics.rkt'
path_to_test_compile = './test-compile.rkt'
path_to_test_dynamics = './test-dynamics.rkt'
path_to_test_optimize = './test-optimize-template.rkt'


def main():
    import os
    # list all tests in the conformance_suite directory
    test_files = [os.path.join(d, f) for d, _, files in os.walk(
        path_to_conformance_suite) for f in files if f.endswith('.py')]
    test_files.sort()
    print(test_files)
    parsed_test_files = [parse_python_file(x) for x in test_files]
    # output test_grammar.rkt
    with open(path_to_test_grammar, 'w') as f:
        f.write('\n'.join([
            '#lang racket',
            '(require "grammar.rkt")',
            '(require redex/reduction-semantics)',
            ''
        ]))
        for name, spec, prog, source in parsed_test_files:
            test = python_file_to_redex_grammar_test(spec, prog)
            f.write('\n')
            f.write(';; ' + name + '\n')
            f.write(str_of_sexp(test))
            f.write('\n')
    # output test_desugar.rkt
    with open(path_to_test_desugar, 'w') as f:
        f.write('\n'.join([
            '#lang racket',
            '(require "desugar.rkt")',
            '(require redex/reduction-semantics)',
            ''
        ]))
        for name, spec, prog, source in parsed_test_files:
            test = python_file_to_redex_desugar_test(spec, prog)
            f.write('\n')
            f.write(';; ' + name + '\n')
            f.write(str_of_sexp(test))
            f.write('\n')
    # output test_statics.rkt
    with open(path_to_test_statics, 'w') as f:
        f.write('\n'.join([
            '#lang racket',
            '(require redex/reduction-semantics)',
            '(require redex-abbrevs)',
            '(require "grammar.rkt")',
            '(require "desugar.rkt")',
            '(require "statics.rkt")',
            ''
        ]))
        for name, spec, prog, source in parsed_test_files:
            test = python_file_to_redex_static_test(spec, prog)
            f.write('\n')
            f.write(';; ' + name + '\n')
            f.write(str_of_sexp(test))
            f.write('\n')
    # output test_compile.rkt
    with open(path_to_test_compile, 'w') as f:
        f.write('\n'.join([
            '#lang racket',
            '(require rackunit)',
            '(require redex/reduction-semantics)',
            '(require "desugar.rkt")',
            '(require "compile.rkt")',
            ''
        ]))
        for name, spec, prog, source in parsed_test_files:
            test = python_file_to_redex_compile_test(spec, prog)
            f.write('\n')
            f.write(';; ' + name + '\n')
            f.write(str_of_sexp(test))
            f.write('\n')
    # output test_dynamics.rkt
    with open(path_to_test_dynamics, 'w') as f:
        f.write('\n'.join([
            '#lang racket',
            '(require redex/reduction-semantics)',
            '(require redex-abbrevs)',
            '(require "desugar.rkt")',
            '(require "compile.rkt")',
            '(require "dynamics.rkt")',
            ''
        ]))
        for name, spec, prog, source in parsed_test_files:
            if spec['compile'] and (spec['run'] is not None):
                test = python_file_to_redex_dynamic_test(spec, prog)
                f.write('\n')
                f.write(';; ' + name + '\n')
                f.write(str_of_sexp(test))
                f.write('\n')
    # output test_optimize.rkt
    with open(path_to_test_optimize, 'w') as f:
        f.write('\n'.join([
            '#lang racket',
            '(require redex/reduction-semantics)',
            '(require redex-abbrevs)',
            '(require "desugar.rkt")',
            '(require "compile.rkt")',
            '',
            '(define-language Matcher',
            '  (Any hole',
            '  (any ... Any any ...)))',
            ''
        ]))
        for name, spec, prog, source in parsed_test_files:
            if spec['compile'] and spec['optimization']:
                test = python_file_to_redex_optimization_test(spec, prog)
                f.write('\n')
                f.write(';; ' + name + '\n')
                f.write(str_of_sexp(test))
                f.write('\n')
                f.write('#|\n')
                f.writelines(source[4:])
                f.write('|#\n')
                f.write('\n')


main()
