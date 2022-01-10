# test_compile_dict_get.py
# This should pass.
# This should terminate.

# This whole test is created manually because our script can't catch a second `codestr`.
# We ignore the first `codestr` because we type `CheckedDict[T,T].get` as `dynamic`.
# The `.get` method takes an optional argument, and the type system in our model can't
# express optional argument.

from __static__ import CheckedDict
class B: pass
class D(B): pass
def testfunc():
    x = CheckedDict[B, int]({B():42, D():42})
    return x

def main(test):
    assert type(test()) == CheckedDict[B,int]

main(testfunc)
# def test_compile_dict_get(self):
#     codestr = """
#         from __static__ import CheckedDict
#         def testfunc():
#             x = CheckedDict[int, str]({42: 'abc', })
#             x.get(42, 42)
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         r"Literal\[42\] received for positional arg 2, expected Optional\[str\]",
#     ):
#         self.compile(codestr, modname="foo")
#     codestr = """
#         from __static__ import CheckedDict
#         class B: pass
#         class D(B): pass
#         def testfunc():
#             x = CheckedDict[B, int]({B():42, D():42})
#             return x
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         B = mod.B
#         self.assertEqual(type(test()), chkdict[B, int])
