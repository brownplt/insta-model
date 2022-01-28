"""
This program processes tests.py, which is the official conformance suite of 
Static Python. It generates tests written in our format and put them in 
./conformance_suite and ./skipped_tests.
"""
import glob
import json
import ast
from os import stat
from re import match
import re

input_file = "./tests.py"
skip_prefix = "    @skipIf("
test_prefix = "    def test_"
output_path_prefix = "./conformance_suite/"
# Ignore a test if it contains one of the following word
ban_anywhere_in_test = list({
    # We don't model floats.
    #   float are fairly broken. it is not a super class of int
    #   many systems consider float a super type, but in fact it is not
    #   because .is_integer is not in int.
    'float': ['float'],
    # We don't model Python's weird scoping rules.
    'python_scope': ['nonlocal',
                     'global', ],
    # We don't model C types.
    'C_types': ['double',
                'int8',
                'int32',
                'int64',
                'box',
                'cbool',
                'ssize_t',
                'Array',
                ],
    # We don't model complicated argument specifications.
    'fancy_args': [
        '\\*args',
        'stararg',
        'default_arg',
        'dstararg',
        '_kw',
        'mixed_args',
        'vararg',
        #   These tests uses keyword argument
        'test_compile_checked_dict_from_dict_call',
        'test_inline_bare_return',
        'test_inline_func_default',
        'test_compile_checked_dict_bad_annotation',
        'test_fast_len_dict_subclass',
        #   These tests uses default argument
        'test_verify_lambda_keyword_only',
        'test_default_type_error',
        'test_check_args_4',
        'test_check_args_5',
    ],
    'async_methods': [
        # We don't model asynchronized methods.
        'await',
        'async',
    ],
    'byte_strings': [
        "bytes"
    ],
    # We don't model format strings.
    'format_strings': [
        'f"[^\\"\n]*"',
        "f'[^\\'\n]*'",
    ],
    'nested_class': [
        # We don't model nested classes.
        #   The SP team wants to ban them too.
        'nested_class',
    ],
    'decorator': [
        # We don't model decorators
        'decorator'
    ],
    # We don't model list/dict/tuple/generator comprehensions.
    'comprehension_syntax': [
        '_comprehension',
        '_comprehension_',
        '_comprehensions_',
        'test_compile_checked_dict_wrong_unknown_type\\(',  # dict
        'test_for_iter_list\\(',  # list
        'test_for_iter_sequence_orelse\\(',  # list
        'test_for_iter_sequence_return\\(',  # list
        'test_nested_for_iter_sequence\\(',  # list
        'test_nested_for_iter_sequence_return\\(',  # list
        'test_for_iter_tuple\\(',  # list
        'test_invoke_with_cell\\(',  # list
        'test_fast_len_conditional_dict\\(', # dict
        'test_fast_len_conditional_dict_funcarg\\(', # dict
        'test_fast_len_conditional_list\\(', # list
        'test_fast_len_conditional_list_funcarg\\(', # list
        'test_fast_len_conditional_set\\(', # set
        'test_fast_len_conditional_set_funcarg\\(', # set
        'test_fast_len_conditional_tuple\\(', # tuple
        'test_fast_len_conditional_tuple_funcarg\\(', # tuple
        'test_fast_len_loop_conditional_list\\(', # list
        'test_fast_len_loop_conditional_dist\\(', # dist
        'test_fast_len_loop_conditional_set\\(', # set
        'test_fast_len_loop_conditional_tuple\\(', # tuple
        'test_invoke_with_cell_arg\\(', # list
    ],
    'memory_management': [
        # We don't model memory management.
        'test_max_stability',
        'test_min_stability',
        '__del__',
    ],
    'fancy_function_type': [
        # We don't model function types that include argument names.
        'test_incompat_override_method_arg_name',
    ],
    'code_flag': [
        # We don't model code flag.
        'test_code_flags',
    ],
    'try_except_redeclare': [
        # We don't model this special case of redeclaration.
        #   Redeclaration is generally banned. We don't want to allow this
        #   special case.
        'test_assign_try_except_typing_redeclared_after'
    ],
    'slicing_syntax': [
        # We don't model slicing syntax.
        'test_for_iter_list_modified\\('
    ],
    # We don't model constants.
    'immutable_variables': [
        'Final',
        'Final\\['
    ],
    'final_classes': [
        # We don't model finalized classes.
        '@final'
    ],
    'advanced_occurrance_typing': [
        # We don't model break and continue.
        #   The challenges lie in the occurrance typing, not at runtime.
        #   Our runtime does support break and continue. In fact, we desugar for-loops
        #   to while-loops that involve break and continue.
        'break',
        'continue',
        # This test uses `return` in a the body of a while-loop, which also makes the
        #   occurrance typing problem more challenging.
        'test_assign_while_returns_but_assigns_first'
    ],
    'TypeVar': [
        # We don't model type variables.
        'TypeVar'
    ],
    # We don't model these things as well.
    '...': ['\\.\\.\\.'],
    'NamedTuple': ['NamedTuple'],
    'with_traceback': ['with_traceback'],
    'sys.modules': ['sys\\.modules'],
    'Protocol': ['Protocol'],
    'prod_assert': ['prod_assert'],
    'sorted': ['sorted'],
    'shadow_frame': ['shadow_frame'],
    '__setattr__': ['__setattr__'],
    '__slots__': ['__slots__'],
    'reveal_type': ['reveal_type'],
    'xxclassloader': ['xxclassloader'],
    'weakref': ['weakref'],
    '@_donotcompile': ['@_donotcompile'],
    '@staticmethod': ['@staticmethod'],
    '@property': ['@property'],
    'optimize=1': ['optimize=1'],
    '__call__': ['__call__'],
    'bad_tests': ['test_break_condition'],
    'nonexpressible_tests': [
        'test_override_bad_ret\(',
        'test_invoke_dict_override\('
    ],
}.items())

hand_translated_prefix = './conformance_suite/edited_'
hand_translated_tests = glob.glob('{}*'.format(hand_translated_prefix))
hand_translated_tests = [s[len(hand_translated_prefix):-3]
                         for s in hand_translated_tests]


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


def translate_simple_pass_compile_test(name, test):
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
    if test.endswith(pass_spec1) or test.endswith(pass_spec2) or test.endswith(pass_spec3):
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


def translate_simple_fail_compile_test(name, test):
    # This group is good. It only translates compilation tests.
    # And we keep all information about compilation tests.
    code, spec = parse_simple_test(test)
    if "TypedSyntaxError" in test:
        content = '\n'.join([
            '# {}.py'.format(name),
            '# This should fail.',
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
    assert str(func_value.id) == "self"
    assert func_attr == "type_error"

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


def parse_asserts(name, spec):
    imports = []
    actions = []

    def preprocess(e: str):
        e = e.replace('chkdict', 'CheckedDict')
        e = e.replace('mod.', '')
        return e

    def rec(node):
        nonlocal actions, imports
        if isinstance(node, list):
            for x in node:
                rec(x)
            return
        if isinstance(node, ast.Assign):
            return
        if isinstance(node, ast.Expr):
            rec(node.value)
            return
        if isinstance(node, ast.Call):
            node_as_str = ast.unparse(node)
            if node_as_str.startswith('self.assertInBytecode'):
                return
            if node_as_str.startswith('self.assertNotInBytecode'):
                return
        if isinstance(node, ast.With):
            node_as_str = ast.unparse(node)
            if node_as_str.split('\n')[0].endswith('as mod:'):
                for s_as_stmt in node.body:
                    s_as_str = ast.unparse(s_as_stmt)
                    if s_as_str.startswith('self.assertTrue'):
                        continue
                    if s_as_str.startswith('self.assertFalse'):
                        continue
                    if s_as_str.startswith('self.assert_jitted'):
                        continue
                    if s_as_str.startswith('self.assert_not_jitted'):
                        continue
                    if s_as_str.startswith('self.assertInBytecode'):
                        continue
                    if s_as_str.startswith('self.assertNotInBytecode'):
                        continue
                    if s_as_str.startswith('if cinderjit'):
                        continue
                    if s_as_str.startswith('self.assertEqual'):
                        assert isinstance(s_as_stmt, ast.Expr)
                        value = s_as_stmt.value
                        assert isinstance(value, ast.Call)
                        lft = ast.unparse(value.args[0])
                        rht = ast.unparse(value.args[1])
                        actions.append("assert {} == {}".format(
                            preprocess(lft), preprocess(rht)))
                        continue
                    if isinstance(s_as_stmt, ast.Assign):
                        targets = s_as_stmt.targets
                        assert len(targets) == 1

                        target = ast.unparse(targets[0])
                        source = ast.unparse(s_as_stmt.value)
                        if 'mod.' in source:
                            source = preprocess(source)
                            imports.append((source, target))
                            continue
                        else:
                            source = preprocess(source)
                            if target != source:
                                actions.append(
                                    "{} = {}".format(target, source))
                            continue
                    if isinstance(s_as_stmt, ast.ClassDef):
                        actions += s_as_str.split('\n')
                        continue
                    if isinstance(s_as_stmt, ast.Expr):
                        actions.append(s_as_str)
                        continue
                    if isinstance(s_as_stmt, ast.For):
                        # actions.append(s_as_str)
                        rec(s_as_stmt.body)
                        continue
                    if isinstance(s_as_stmt, ast.Delete):
                        actions.append(s_as_str)
                        continue
                    if s_as_str.startswith('with self.assertRaises('):
                        assert isinstance(s_as_stmt, ast.With)
                        assert len(s_as_stmt.items) == 1
                        item = s_as_stmt.items[0]
                        context_expr = item.context_expr
                        assert isinstance(context_expr, ast.Call)
                        args = context_expr.args
                        arg0_as_node = args[0]
                        arg0_as_str: str = ast.unparse(arg0_as_node)
                        if arg0_as_str in {'AssertionError', 'TypeError', 'AttributeError'}:
                            body = s_as_stmt.body
                            assert len(body) == 1
                            s = ast.unparse(body[0])
                            actions = actions + [
                                'try:',
                                '    {}'.format(s),
                                'except {}:'.format(arg0_as_str),
                                '    pass',
                                'else:',
                                '    raise Exception()'
                            ]
                            continue
                    if s_as_str.startswith('with self.assertRaisesRegex('):
                        assert isinstance(s_as_stmt, ast.With)
                        assert len(s_as_stmt.items) == 1
                        item = s_as_stmt.items[0]
                        context_expr = item.context_expr
                        assert isinstance(context_expr, ast.Call)
                        args = context_expr.args
                        arg0_as_node = args[0]
                        arg0_as_str: str = ast.unparse(arg0_as_node)
                        if arg0_as_str in {'AssertionError', 'TypeError', 'AttributeError'}:
                            body = s_as_stmt.body
                            assert len(body) == 1
                            s = ast.unparse(body[0])
                            actions = actions + [
                                'try:',
                                '    {}'.format(s),
                                'except {}:'.format(arg0_as_str),
                                '    pass',
                                'else:',
                                '    raise Exception()'
                            ]
                            continue
                    assert False
                return
        assert False

    rec(spec)

    if all(isinstance(s, str) for s in actions):
        body = '\n'.join(['    {}'.format(s) for s in actions]) + '\n'
        return '\n'.join([
            'def main({}):'.format(
                ', '.join(target for source, target in imports)),
            body,
            'main({})'.format(', '.join(source for source, target in imports))
        ])
    else:
        print("Internal error 4")
        exit(1)


def translate_all_assert_tests(name, test):
    code, spec = parse_simple_test(test)
    asserts = [
        'assertEqual',
        'assertRaises'
    ]
    assert any(word in test for word in asserts)

    actions = parse_asserts(name, spec)

    content = '\n'.join([
        '# {}.py'.format(name),
        '# This should pass.',
        '# This should terminate.',
        '',
        ''
    ]) + code + '\n' + actions

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
    # Translate all these test to compilation test.
    # We don't claim that we perform the same optimizations, but at least those
    # programs type check.

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


# def record_skipped_test(name, test, reason):
#     reason_count[reason] = reason_count.get(reason, 0) + 1
#     skipped_tests_path = skipped_tests_path_prefix + name + ".py"
#     skipped_tests_file = open(skipped_tests_path, 'w')
#     skipped_tests_file.write("# Reason: {}\n".format(reason))
#     skipped_tests_file.write(test)
#     return


def main():
    left_out_tests = {}
    for test in read_tests(input_file):
        name = get_name(test)

        row = {}
        import re
        for category, words in ban_anywhere_in_test:
            row[category] = int(any(re.search(w, test) is not None for w in words))
        if any(i == 1 for i in row.values()):
            row['model_could_not_parse'] = 0
            while name in left_out_tests.keys():
                name = name + ' (again)'
            left_out_tests[name] = row
            continue

        if name in hand_translated_tests:
            continue

        translators = [
            translate_simple_fail_compile_test,
            translate_simple_pass_compile_test,
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
                continue
        if not translated:
            row['model_could_not_parse'] = 1
            while name in left_out_tests.keys():
                name = name + ' (again)'
            left_out_tests[name] = row
            continue
    import csv
    with open('left-out_reason.csv', 'w') as csvfile:
        fieldnames = ['test_name'] + [c for c,
                                      w in ban_anywhere_in_test] + ['model_could_not_parse']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for name, row in left_out_tests.items():
            row['test_name'] = name
            writer.writerow(row)
    return


main()
