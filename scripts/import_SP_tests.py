"""
This program processes test.rkt, which is the official conformance suite of 
Static Python. It generates tests written in our format and put them in 
./conformance_suite.
"""
import ast
from os import write
from types import BuiltinMethodType
from unittest.case import skip

input_file = "./tests.py"
skip_prefix = "    @skipIf("
test_prefix = "    def test_"
output_path_prefix = "./conformance_suite/"
skipped_tests_path_prefix = "./skipped_tests/"


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
    #         one_statement_that_specify_what_to_check
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
    pass_spec1 = '\n'.join([
        '',
        '    self.compile(codestr)',
        ''
    ])
    pass_spec2 = '\n'.join([
        '',
        '    self.compile(codestr, modname="foo")',
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
    elif test.endswith(pass_spec1) or test.endswith(pass_spec2):
        content = '\n'.join([
            '# {}.py'.format(name),
            '# This should pass.',
            '',
            ''
        ]) + code
    else:
        assert False

    commented_src = '\n' + '\n'.join('# ' + line for line in test.splitlines())
    content += commented_src + '\n'
    return (name, content)


def translate_less_simple_compile_test(test: str):
    # Capture tests that look like
    #     def test_name(self) -> None:
    #         codestr = """
    #             some code
    #         """
    #         with ....:
    #             self.compile(codestr)
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
    # assert len(body.args) in {1, 2}
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
    commented_src = '\n' + '\n'.join('# ' + line for line in test.splitlines())
    content += commented_src + '\n'
    return (name, content)


for test in read_tests(input_file):
    if 'reveal_type' in test: continue 
    if 'nonlocal' in test: continue
    if 'global' in test: continue
    if '...' in test: continue
    if 'double' in test: continue
    if 'int8' in test: continue
    if 'int32' in test: continue
    if 'int64' in test: continue
    if 'box' in test: continue
    if 'cbool' in test: continue
    if 'Array' in test: continue
    if '*args' in test: continue

    lines = test.split('\n')
    first_line = lines[0]
    try:
        # This line actually skips a bunch of tests.
        name = first_line[4:first_line.index("(self)")]
        if '_kw' in name: continue
        if 'mixed_args' in name: continue
        if 'varargs' in name: continue
        if 'test_if_else_optional_return_two_branches' in name: continue
        # ⬆️ This test uses an unbound identifier
    except Exception:
        continue

    # Skip more
    try:
        # TODO: these are tests that we care but don't support right now.
        assert not 'while' in test
        assert not '@staticmethod' in test
        assert not '@final' in test
        assert not 'assign_chained' in name
        assert not 'chain_assign' in name
        try:
            (name, content) = translate_simple_compile_test(test)
        except AssertionError:
            (name, content) = translate_less_simple_compile_test(test)
        output_path = output_path_prefix + name + ".py"
        output_file = open(output_path, 'w')
        output_file.write(content)
        print(test)
        print('-' * 10)
    except Exception:
        print("SKIPPED", name)
        skipped_tests_path = skipped_tests_path_prefix + name + ".py"
        skipped_tests_file = open(skipped_tests_path, 'w')
        skipped_tests_file.write(test)
    
