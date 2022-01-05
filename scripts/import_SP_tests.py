"""
This program processes tests.py, which is the official conformance suite of 
Static Python. It generates tests written in our format and put them in 
./conformance_suite and ./skipped_tests.
"""
import json
import ast
from os import stat
from re import match

input_file = "./tests.py"
skip_prefix = "    @skipIf("
test_prefix = "    def test_"
output_path_prefix = "./conformance_suite/"
skipped_tests_path_prefix = "./skipped_tests/"
cannot_parse_path_prefix = "./cannot_parse/"
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
    # format string
    'f"',
    "f'",
    # Not even implemented in SP
    "@skipIf(True, \"this isn't implemented yet\")",
    # more powerful features.
    'NamedTuple',
    # ban nested classes
    'nested_class',

    # These test uses an unbound identifier
    'test_if_else_optional_return_two_branches',
    'test_assign_try_except_redeclare_unknown_type',
    'test_untyped_attr',
    'test_assign_num_to_dynamic',
    'test_assign_dynamic_to_dynamic',
    'test_verify_arg_unknown_type',
    'test_override_override_inherited',

    # These test uses keyword argument
    'test_compile_checked_dict_from_dict_call',
    'test_inline_bare_return',
    'test_inline_func_default',
    'test_compile_checked_dict_bad_annotation',

    # This test uses default argument
    'test_verify_lambda_keyword_only',
    'test_default_type_error',
    'test_check_args_4',
    'test_check_args_5',

    # This test uses dict comprehension.
    'test_compile_checked_dict_wrong_unknown_type',

    # This test uses string literal to write Optional type...
    'test_call_function_unknown_ret_type',

    # We don't support this.
    'test_incompat_override_method_arg_name',

    # The scope is funny
    'test_assign_try_except_typing_redeclared_after',

    # This test is bad
    'test_break_condition',

    # fancy argment spec
    'test_method_prologue_posonly', 'test_check_args_6', 'test_check_args_7',

    # code flag
    'test_code_flags',

    # These tests have been hand-translated.
    'test_checked_dict',
    'test_compile_dict_get',
    'test_compile_method',
    'test_error_incompat_return',
    'test_strict_module_isinstance',
    'test_typed_field_deleted_attr',
    'test_verify_arg_dynamic_type',
    'test_widen_to_dynamic'
]

skip_anywhere_in_test = [
    '@staticmethod',
    'break', 'continue',
    '@final',
    'Final',
    'Final[',
    'Protocol',
    'prod_assert',
    '@property',
    '__static__.compiler_flags',
    '__setattr__',
    '__slots__',
    'reveal_type',
    'test_sorted',  # also for-loop
    'test_for_iter_list_modified(',  # slicing
    'test_for_iter_list(',  # list comprehension
    'test_for_iter_sequence_orelse(',  # list comprehension
    'test_for_iter_sequence_return(',  # list comprehension
    'test_nested_for_iter_sequence(',  # list comprehension
    'test_nested_for_iter_sequence_return(',  # list comprehension
    'test_for_iter_tuple(',  # list comprehension
    'xxclassloader',
    'weakref',
    '@_donotcompile',
    '_comprehension',
    '_comprehension_',
    '_comprehensions_',
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


def get_name(test):
    # A simple test is a test that looks like
    #     def test_name(self):
    #         codestr = """
    #         code goes here
    #         """
    #         statements that specify what to check

    lines = test.split('\n')
    if lines[0].startswith('@skip'):
        lines = lines[1:]
    first_line = lines[0]
    name = first_line[4:first_line.index("(self)")]
    return name


def parse_simple_test(test):
    # A simple test is a test that looks like
    #     def test_name(self):
    #         codestr = """
    #         code goes here
    #         """
    #         statements that specify what to check
    parsed_test = ast.parse(test, type_comments=True)

    assert isinstance(parsed_test, ast.Module)
    assert len(parsed_test.body) == 1
    deffun = parsed_test.body[0]
    assert isinstance(deffun, ast.FunctionDef)
    body = deffun.body
    assert len(body) > 1
    if isinstance(body[0], ast.Expr):
        body = body[1:]
    defcode = body[0]
    spec = body[1:]
    assert isinstance(defcode, ast.Assign)

    # process code
    assert len(defcode.targets) == 1
    assert isinstance(defcode.targets[0], ast.Name)
    assert str(defcode.targets[0].id) in {
        "codestr", "code"}
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


def translate_simple_compile_test(name, test):
    # This group is good. It only translates compilation tests.
    # And we keep all information about compilation tests.
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
    return content


def translate_self_type_error_test(name, test):
    code, spec = parse_simple_test(test)
    # Capture tests that look like
    #     def test_name(self) -> None:
    #         codestr = """
    #             some code
    #         """
    #         self.type_error(...)
    assert len(spec) == 1
    spec = spec[0]

    assert isinstance(spec, ast.Expr)
    spec = spec.value
    assert isinstance(spec, ast.Call)
    func = spec.func
    assert isinstance(func, ast.Attribute)
    func_value = func.value
    func_attr = func.attr
    assert isinstance(func_value, ast.Name)
    assert str(func_value.id) is "self"
    assert func_attr is "type_error"

    content = '\n'.join([
        '# {}.py'.format(name),
        '# This should fail.',
        '',
        ''
    ]) + code
    commented_src = '\n' + '\n'.join('# ' + line for line in test.splitlines())
    content += commented_src + '\n'
    return content


def translate_with_compile_test(name, test):
    code, spec = parse_simple_test(test)
    # Capture tests that look like
    #     def test_name(self) -> None:
    #         codestr = """
    #             some code
    #         """
    #         with ....:
    #             self.compile(codestr)
    assert len(spec) == 1
    spec = spec[0]

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
    return content

def split_items(matched_string):
    import ast
    m = ast.parse('({})'.format(matched_string))
    e1, e2 = m.body[0].value.elts
    return ast.unparse(e1), ast.unparse(e2)

def translate_all_assert_tests(name, test):
    code, spec = parse_simple_test(test)
    # Capture tests that have some assertEqual/assertRaise ... (TODO)
    asserts = [
        'assertEqual',
    ]
    assert any(word in test for word in asserts)
    
    assertions = {
        'assertEqual': []
    }

    if 'f = mod.testfunc' in test:
        code += '\n' + 'f = testfunc'
    if 'test = mod.testfunc' in test:
        code += '\n' + 'test = testfunc'
    if 'f = mod.func' in test:
        code += '\n' + 'f = func'

    import re
    pattern = 'self\\.assertEqual\\((.*)\\)\n'
    assertEqual_matches = re.findall(pattern, test)
    for matched_string in assertEqual_matches:
        matched_string: str = matched_string.replace('chkdict', 'CheckedDict')
        try:
            lft_e, rht_e = split_items(matched_string)
            if lft_e.startswith('mod.'):
                lft_e = lft_e[len('mod.'):]
            code += '\n' + 'assert {} == {}'.format(lft_e, rht_e)
        except Exception:
            code += '\n' + '# self.assertEqual({})'.format(matched_string)

    code += '\n'
    
    content = '\n'.join([
        '# {}.py'.format(name),
        '# This should pass.',
        '# This should terminate.',
        '',
        ''
    ]) + code

    commented_src = '\n' + '\n'.join('# ' + line for line in test.splitlines())
    content += commented_src + '\n'
    return content

def translate_optimization_test(name, test):
    code, spec = parse_simple_test(test)
    # Capture tests that look like
    #     def test_name(self) -> None:
    #         ...
    #         assertInByteCode/assertNotInByteCode/assertTrue/assertFalse
    #         ...

    asserts = [
        'assertInBytecode',
        'assertNotInBytecode',
        'assertTrue',
        'assertFalse'
    ]
    assert any(word in test for word in asserts)

    content = '\n'.join([
        '# {}.py'.format(name),
        '# This should pass.',
        '',
        ''
    ]) + code
    commented_src = '\n' + '\n'.join('# ' + line for line in test.splitlines())
    content += commented_src + '\n'
    return content

reason_count = {}


def record_skipped_test(name, test, reason):
    reason_count[reason] = reason_count.get(reason, 0) + 1
    print('/' + '-' * 10 + '\\')
    print("SKIPPED", name)
    print(test)
    print('\\' + '-' * 10 + '/')
    skipped_tests_path = skipped_tests_path_prefix + name + ".py"
    skipped_tests_file = open(skipped_tests_path, 'w')
    skipped_tests_file.write("# Reason: {}\n".format(reason))
    skipped_tests_file.write(test)
    return


def main():
    banned_counter = 0
    imparsable_counter = 0
    for test in read_tests(input_file):
        banned = None
        for word in ban_anywhere_in_test:
            if word in test:
                banned = word
                break
        if banned is not None:
            # record_skipped_test("banned_test_{}".format(banned_counter), test, "Test hitted a banned word {}".format(banned))
            banned_counter += 1
            continue

        try:
            name = get_name(test)
        except Exception as e:
            print("<SKIPPED begin")
            print(test)
            print("<SKIPPED end")
            record_skipped_test("imparsable_test_{}".format(
                imparsable_counter), test, "Format too complicated")
            imparsable_counter += 1
            continue

        skipped = False
        for word in skip_anywhere_in_test:
            if word in test:
                skipped = True
                record_skipped_test(
                    name, test, "Hitted a skipped word ({})".format(word))
                break
        if skipped:
            continue

        translators = [
            translate_simple_compile_test,
            translate_self_type_error_test,
            translate_with_compile_test,
            translate_all_assert_tests,
            translate_optimization_test
        ]
        translated = False
        for tr in translators:
            try:
                content = tr(name, test)
                output_path = output_path_prefix + name + ".py"
                output_file = open(output_path, 'w')
                output_file.write(content)
                translated = True
                break
            except Exception as e:
                print("WHAT?", repr(e))
                continue
        if not translated:
            record_skipped_test(
                name, test, "Can't be translated by any of the three translator")


main()
reason_count = list(sorted([(v, k)
                    for k, v in reason_count.items()], reverse=True))
print(json.dumps(reason_count, indent=2))
