"""
This program processes tests.py, which is the official conformance suite of 
Static Python. It generates tests written in our format and put them in 
./conformance_suite and ./skipped_tests.
"""
import ast

input_file = "./tests.py"
skip_prefix = "    @skipIf("
test_prefix = "    def test_"
output_path_prefix = "./conformance_suite/"
skipped_tests_path_prefix = "./skipped_tests/"
# Ignore a test if it contains one of the following word
ban_anywhere_in_test = [
    # float are fairly broken
    #   it is not a super class of int
    #   many systems consider float a super type, but in fact it is not
    #   because .is_integer is not in int.
    'float',
    # Weird python scope
    'nonlocal',
    'global',
    # Not useful for our purposes
    '...',
    # C types
    'double',
    'int8',
    'int32',
    'int64',
    'box',
    'cbool',
    'Array',
    # Rest arguments
    '*args',
    'stararg',
    'default_arg',
    'dstararg',
    '_kw',
    'mixed_args',
    'vararg',
    # async_method
    'await',
    'async',
    # Byte string
    'b"',
    "b'",
    # Not even implemented in SP
    "@skipIf(True,",
    # Some weird module
    'xxclassloader',
    # more powerful features.
    'NamedTuple',
    # We don't spend too much time on occurrence typing
    'break', 'continue'
]
ban_in_test_name = [
    'test_if_else_optional_return_two_branches',
    # ⬆️ This test uses an unbound identifier
    'test_compile_checked_dict_from_dict_call',
    # This test uses keyword argument
    'test_inline_bare_return',
    # This test uses keyword argument
    'test_inline_func_default',
    # This test uses default argument
    'test_verify_lambda_keyword_only',
    # This test uses keyword argument
    'test_compile_checked_dict_bad_annotation',
    # The annotation is too bad... it uses 42 as type.
    'test_compile_checked_dict_wrong_unknown_type',
    # This test uses dict comprehension.
    'test_call_function_unknown_ret_type',
    # This test uses an unbound identifier in type annotation
    'test_override_override_inherited',
    # This test uses string literal to write Optional type...
    'test_incompat_override_method_arg_name',
    # We don't support this.
    'test_compile_nested_class_in_fn',
    # nested class
    'test_assign_try_except_redeclare_unknown_type',
    # Unbound identifier
    'test_assign_try_except_typing_redeclared_after',
    # The scope is funny
    'test_break_condition',
    # This test is bad
    'test_method_prologue_posonly',
    # fancy argment spec
]
skip_anywhere_in_test = [
    # Skip for now
    '@staticmethod',
    # To confirm
    'Protocol',
    'prod_assert',
    '@property',
    '__static__.compiler_flags',
    'weakref',
    '@_donotcompile',
    '__setattr__',
    '__slots__',
    'reveal_type',
    'test_compile_checked_dict_with_annotation_comprehension', # comprehension
    'test_for_iter_list_modified', # slice
]
skip_in_code = [
    '@final',
    'Final[',
    'Final',
]


def read_tests(file_path):
    f = open(file_path, 'r')
    acc = None
    lines = f.readlines()

    def test_begins(l):
        return l.startswith(test_prefix) or l.startswith(skip_prefix)

    def test_ends(l):
        return l.startswith('class ') or l.startswith('if __name__ == "__main__') or test_begins(l)

    def acc_to_test(acc):
        # skip the first 4 whitespace
        for line in acc:
            assert line[:4].strip() == "", "the line is {}".format(repr(line))
        return "".join(map(lambda line: line[4:], acc))

    acc = []
    for line in lines:
        # I said `> 1` because it might be that there is a @skip and then
        # a `def test_`. If I said `> 0`, the skip line will be thought
        # as a test alone
        if test_ends(line) and len(acc) > 1:
            yield acc_to_test(acc)
            acc = []

        if test_begins(line):
            acc = [line]
        elif len(acc) > 0:
            # Only if the new tests have started
            acc.append(line)

    if len(acc) > 0:
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
    pass_spec3 = '\n'.join([
        '',
        '    code = self.compile(codestr, modname="foo")',
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
    elif test.endswith(pass_spec1) or test.endswith(pass_spec2) or test.endswith(pass_spec3):
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
    # assert body.keywords == []
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


def translate_optimization_test(test: str):
    # Capture tests that look like
    #     def test_name(self) -> None:
    #         codestr = """
    #             some code
    #         """
    #         with ....:
    #             ....
    code, spec = parse_simple_test(test)

    # assert 'assertInBytecode' in test
    # assert 'INVOKE_FUNCTION' in test

    assert isinstance(spec, ast.With)

    content = '\n'.join([
        '# {}.py'.format(name),
        '# This should pass.',
        '# This should terminate.',
        '# This should be optimized.' if 'assertInBytecode' in test else '',
        '',
        ''
    ]) + code
    commented_src = '\n' + '\n'.join('# ' + line for line in test.splitlines())
    content += commented_src + '\n'
    return (name, content)


def record_skipped_test(name, test, reason):
    print('/' + '-' * 10 + '\\')
    print("SKIPPED", name)
    print(test)
    print('\\' + '-' * 10 + '/')
    skipped_tests_path = skipped_tests_path_prefix + name + ".py"
    skipped_tests_file = open(skipped_tests_path, 'w')
    skipped_tests_file.write("# Reason: {}\n".format(reason))
    skipped_tests_file.write(test)
    return


for test in read_tests(input_file):
    if any(word in test for word in ban_anywhere_in_test):      
        continue

    for word in ban_anywhere_in_test:
        assert not (word in test)

    lines = test.split('\n')
    if lines[0].startswith('@skip'):
        lines = lines[1:]
    first_line = lines[0]
    name = first_line[4:first_line.index("(self)")]

    if any(word in name for word in ban_in_test_name):
        continue

    for word in ban_in_test_name:
        assert not (word in name)

    if any(word in test for word in skip_anywhere_in_test):
        record_skipped_test(name, test, "Test hitted some skipped words")
        continue

    for word in skip_anywhere_in_test:
        assert not (word in test)
    
    print("word", word)
    print("test", test)
    print("skip_anywhere_in_test", skip_anywhere_in_test)
    assert not name == "test_exact_float_type"
    try:
        code, spec = parse_simple_test(test)
    except Exception:
        record_skipped_test(name, test, "Format too complicated")
        continue
    
    if any(word in code for word in skip_in_code):
        record_skipped_test(name, test, "Code hitted some skipped words")
        continue

    parsers = [
        translate_simple_compile_test,
        translate_less_simple_compile_test,
        translate_optimization_test
    ]
    for parser in parsers:
        try:
            (name, content) = parser(test)
            output_path = output_path_prefix + name + ".py"
            output_file = open(output_path, 'w')
            output_file.write(content)
            break
        except Exception:
            continue
    else:
        # If broken out, we won't reach here.
        try:
            code, spec = parse_simple_test(test)
            record_skipped_test(name, test, "Can't be translated by any of the three translator")
        except Exception:
            record_skipped_test(name, test, "Can't even parse")
            continue
