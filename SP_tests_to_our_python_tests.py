"""
This program processes test.rkt, which is the official conformance suite of 
Static Python. It generates tests written in our format and put them in 
./conformance_suite.
"""
import ast
from os import write
from types import BuiltinMethodType
from unittest.case import skip

input_file = "tests.py"
skip_prefix = "    @skipIf("
test_prefix = "    def test_"
output_path_prefix = "./conformance_suite/"


def read_tests(file_path):
    f = open(file_path, 'r')
    acc = None
    lines = f.readlines()

    def new_test_begins(l):
        return l.startswith(test_prefix) or l.startswith(skip_prefix)

    def acc_to_test(acc):
        # skip the first 4 whitespace
        return "".join(map(lambda line: line[4:], acc))
    for i in range(len(lines)):
        line = lines[i]
        if new_test_begins(line):
            acc = [line]
            break
    for line in lines[i+1:]:
        if not line.startswith(' ' * 4):
            continue
        if new_test_begins(line):
            yield acc_to_test(acc)
            acc = [line]
        else:
            acc.append(line)
    yield acc_to_test(acc)


def parse_simple_test(test):
    # A simple test is a test that looks like
    #     def test_name(self):
    #         codestr = """
    #         code goes here
    #         """
    #         a_statement_that_specify_what_to_check
    parsed_test = ast.parse(test, type_comments=True)

    assert isinstance(parsed_test, ast.Module)
    assert len(parsed_test.body) == 1
    deffun = parsed_test.body[0]
    assert isinstance(deffun, ast.FunctionDef)
    assert len(deffun.body) == 2
    [defcode, spec] = deffun.body
    assert isinstance(defcode, ast.Assign)

    # process code
    assert len(defcode.targets) == 1
    assert isinstance(defcode.targets[0], ast.Name)
    assert str(defcode.targets[0].id) == "codestr"
    assert isinstance(defcode.value, ast.Constant)
    code = defcode.value.value
    code = code.split('\n')
    # skip the first and last empty line
    assert code[0].strip() == ""
    assert code[-1].strip() == ""
    code = code[1:-1]
    # remove the extra indentation
    while all([line.startswith('    ') for line in code]):
        code = [line[4:] for line in code]
    code = '\n'.join(code)

    # process spec
    pass

    return code, spec


def translate_simple_compile_test(test):
    code, spec = parse_simple_test(test)

    # process spec
    pass_spec = '\n'.join([
        '',
        '    self.compile(codestr)',
        ''
    ])
    fail_spec = '\n'.join([
        '',
        '    with self.assertRaises(TypedSyntaxError):',
        '        self.compile(codestr, modname="foo")',
        ''
    ])
    if test.endswith(fail_spec):
        content = '\n'.join([
            '# {}.py'.format(name),
            '# This should fail.',
            '',
            ''
        ]) + code
    elif test.endswith(pass_spec):
        content = '\n'.join([
            '# {}.py'.format(name),
            '# This should pass.',
            '',
            ''
        ]) + code
    else:
        assert False
    return (name, content)


def translate_less_simple_compile_test(test):
    code, spec = parse_simple_test(test)

    assert isinstance(spec, ast.With)
    assert len(spec.body) == 1
    assert isinstance(spec.body[0], ast.Expr)
    body = spec.body[0].value
    assert isinstance(body, ast.Call)
    assert isinstance(body.func, ast.Attribute)
    assert isinstance(body.func.value, ast.Name)
    assert str(body.func.value.id) == 'self'
    assert str(body.func.attr) == 'compile'
    assert len(body.args) == 1
    assert body.keywords == []
    arg = body.args[0]
    assert isinstance(arg, ast.Name)
    assert str(arg.id) == 'codestr'

    content = '\n'.join([
        '# {}.py'.format(name),
        '# This should fail.',
        '',
        ''
    ]) + code

    return (name, content)


for test in read_tests(input_file):
    lines = test.split('\n')
    first_line = lines[0]
    try:
        name = first_line[4:first_line.index("(self)")]
        # Skip a bunch of things that we don't care
        assert not '_kw' in name
        assert not 'mixed_args' in name
        assert not 'redefine' in name
        assert not 'assign_chained' in name
        # Skip more
        assert not 'reveal_type' in test # TODO
        assert not '@staticmethod' in test # TODO
        assert not 'global' in test
        assert not '...' in test
        assert not 'Optional' in test
        assert not 'while' in test
        assert not ' | ' in test
        assert not 'double' in test
        assert not 'int8' in test
        assert not 'int32' in test
        assert not 'int64' in test
        assert not 'box' in test
        assert not 'cbool' in test
        assert not 'Array' in test
    except AssertionError:
        continue
    except ValueError:
        continue
    try:
        (name, content) = translate_simple_compile_test(test)
    except AssertionError:
        try:
            (name, content) = translate_less_simple_compile_test(test)
        except AssertionError:
            continue

    output_path = output_path_prefix + name + ".py"
    output_file = open(output_path, 'w')
    output_file.write(content)
    print(test)
    print('-' * 10)
    
