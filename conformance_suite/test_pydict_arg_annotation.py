# test_pydict_arg_annotation.py
# This should pass.
# This should terminate.

from __static__ import PyDict
def f(d: PyDict[str, int]) -> str:
    # static python ignores the untrusted generic types
    return d[3]
def main():
    assert f({3: 'foo'}) == 'foo'

main()
# def test_pydict_arg_annotation(self):
#     codestr = """
#         from __static__ import PyDict
#         def f(d: PyDict[str, int]) -> str:
#             # static python ignores the untrusted generic types
#             return d[3]
#     """
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f({3: "foo"}), "foo")
