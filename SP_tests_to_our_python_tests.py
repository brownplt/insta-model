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


def translate_simple_compile_test(test):

    def check_content(parsed_test):
        assert not 'int8' in test
        assert not 'int64' in test
        assert not 'box' in test
        assert not 'cbool' in test
        assert isinstance(parsed_test, ast.Module)
        assert len(parsed_test.body) == 1
        deffun = parsed_test.body[0]
        assert isinstance(deffun, ast.FunctionDef)
        assert len(deffun.body) == 2
        [defcode, specification] = deffun.body
        assert isinstance(defcode, ast.Assign)

        # process code
        assert len(defcode.targets) == 1
        assert isinstance(defcode.targets[0], ast.Name)
        assert str(defcode.targets[0].id) == "codestr"
        assert isinstance(defcode.value, ast.Constant)
        code = defcode.value.value
        code = code.split('\n')
        code = code[1:-1]
        while all([line.startswith('    ') for line in code]):
            code = [line[4:] for line in code]
        code = '\n'.join(code)

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
        # print(repr([test, fail_spec, pass_spec]))
        if test.endswith(fail_spec):
            content = '\n'.join([
                '# {}.py'.format(name),
                '# This should fail.',
                '',
                ''
            ]) + code
            return content
        
        elif test.endswith(pass_spec):
            content = '\n'.join([
                '# {}.py'.format(name),
                '# This should pass.',
                '',
                ''
            ]) + code
            return content
        
        assert False

    lines = test.split('\n')
    first_line = lines[0]
    name = first_line[4:first_line.index("(self)")]
    parsed_test = ast.parse(test, type_comments=True)
    content = check_content(parsed_test)
    return (name, content)


for test in read_tests(input_file):
    try:
        (name, content) = translate_simple_compile_test(test)
        output_path = output_path_prefix + name + ".py"
        output_file = open(output_path, 'w')
        output_file.write(content)
        print(test)
        print('-' * 10)
    except AssertionError:
        pass
    except ValueError:
        pass
    
