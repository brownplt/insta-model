from typing import List
import ast
from os import error


class SExp:
    pass


class SESymbol(SExp):
    def __init__(self, v: str) -> None:
        super().__init__()
        self.v = v

    def __str__(self) -> str:
        return str(self.v)


class SEString(SExp):
    def __init__(self, v: str) -> None:
        super().__init__()
        self.v = v

    def __str__(self) -> str:
        return '"' + repr(self.v)[1:-1] + '"'


class SEBoolean(SExp):
    def __init__(self, v: bool) -> None:
        super().__init__()
        self.v = v

    def __str__(self) -> str:
        return "#t" if self.v else "#f"


class SEInteger(SExp):
    def __init__(self, v: int) -> None:
        super().__init__()
        self.v = v

    def __str__(self) -> str:
        return str(self.v)


class SEList(SExp):
    def __init__(self, v: List[SExp]) -> None:
        super().__init__()
        self.v = v

    def __str__(self) -> str:
        return '(' + ' '.join([str(x) for x in self.v]) + ')'


def expr_to_type(expr: ast.expr) -> SExp:
    if expr is None:
        return SESymbol('dynamic')
    elif isinstance(expr, ast.Name):
        return SESymbol(str(expr.id))
    elif isinstance(expr, ast.Subscript):
        return SEList([expr_to_type(expr.value), expr_to_type(expr.slice)])
    elif isinstance(expr, ast.Constant) and expr.value is None:
        return SESymbol('None')
    else:
        raise Exception("Can't deal with {}".format(expr))


def stmt_to_class_member(stmt: ast.stmt) -> SExp:
    if isinstance(stmt, ast.AnnAssign):
        assert stmt.value is None
        return SEList([
            SESymbol('field'),
            SEString(str(stmt.target.id)),
            expr_to_type(stmt.annotation)])
    elif isinstance(stmt, ast.FunctionDef):
        inputs = SEList([
            SEList([SESymbol(str(a.arg)), expr_to_type(a.annotation)])
            for a in stmt.args.args[1:]])
        output_type = expr_to_type(stmt.returns)
        return SEList([
            SESymbol('method'),
            SEString(str(stmt.name)),
            SESymbol(str(stmt.args.args[0].arg)),
            inputs,
            output_type
        ] + [
            ast_to_sexp(s) for s in stmt.body
        ])
    else:
        raise Exception("Can't deal with {}".format(stmt))


def ast_to_sexp(node) -> SExp:
    if isinstance(node, ast.Module):
        return SEList([ast_to_sexp(e) for e in node.body])
    elif isinstance(node, ast.ClassDef):
        if len(node.bases) == 0:
            base = SESymbol('object')
        elif len(node.bases) == 1:
            base = ast_to_sexp(node.bases[0])
        else:
            raise Exception("Can't deal with multiple base classes")
        return SEList([
            SESymbol('class'),
            SESymbol(node.name),
            base
        ] + [
            stmt_to_class_member(s) for s in node.body if not isinstance(s, ast.Pass)
        ])
    elif isinstance(node, ast.Return):
        if node.value is None:
            return SEList([SESymbol('return'), SESymbol('None')])
        else:
            return SEList([SESymbol('return'), ast_to_sexp(node.value)])
    elif isinstance(node, ast.AnnAssign):
        assert node.value is not None
        return SEList([
            SESymbol('define'),
            SESymbol(str(node.target.id)),
            expr_to_type(node.annotation),
            ast_to_sexp(node.value)
        ])
    elif isinstance(node, ast.Assign):
        assert len(node.targets) == 1
        return SEList([
            SESymbol('define'),
            SESymbol(str(node.targets[0].id)),
            ast_to_sexp(node.value)
        ])
    elif isinstance(node, ast.Pass):
        return SESymbol('pass')
    elif isinstance(node, ast.Expr):
        return ast_to_sexp(node.value)
    elif isinstance(node, ast.Name):
        return SESymbol(str(node.id))
    elif isinstance(node, ast.Constant):
        if isinstance(node.value, str):
            return SEString(node.value)
        elif isinstance(node.value, bool):
            return SEBoolean(node.value)
        elif isinstance(node.value, int):
            return SEInteger(node.value)
        else:
            raise Exception("Can't deal with {}".format(node))
    elif isinstance(node, ast.Call):
        return SEList([ast_to_sexp(node.func)] + [ast_to_sexp(a) for a in node.args])
    assert False, str(node)


def python_file_to_sexp(test_file) -> SExp:
    with open(test_file) as f:
        p = ast.parse(f.read(), type_comments=True)
        return ast_to_sexp(p)


def python_file_to_redex_test(test_file) -> SExp:
    print("Working on " + test_file)
    spec = open(test_file).readlines()[1]
    if spec == '# This should pass.\n':
        check = SESymbol('check-judgment-holds*')
    elif spec == '# This should fail.\n':
        check = SESymbol('check-not-judgment-holds*')
    else:
        assert False, repr(spec)
    return SEList([
        check,
        SEList([
            SESymbol('⊢p'),
            python_file_to_sexp(test_file)
        ])
    ])


def main():
    import os
    # list all tests in the conformance_suite directory
    test_files = [os.path.join(d, f) for d, _, files in os.walk(
        'conformance_suite') for f in files if f.endswith('.py')]
    test_files.sort()
    # output redex test
    with open('conformance_suite.rkt', 'w') as f:
        f.write('\n'.join([
            '#lang racket',
            '(require redex)',
            '(require redex-abbrevs)',
            '(require "model.rkt")',
            '(require "type-check.rkt")',
            ''
        ]))
        for test_file in test_files:
            test = python_file_to_redex_test(test_file)
            f.write('\n')
            f.write(';; ' + test_file + '\n')
            f.write(str(test))
            f.write('\n')


main()
