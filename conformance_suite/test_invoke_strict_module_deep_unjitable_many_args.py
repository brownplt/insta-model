# test_invoke_strict_module_deep_unjitable_many_args.py
# This should pass.
# This should terminate.

def f0(): return 42
def f1(a, b, c, d, e, f, g, h):
    class C: pass
    return f0() - a + b - c + d - e + f - g + h - 4
def f2(): return f1(1,2,3,4,5,6,7,8)
def f3(): return f2()
def f4(): return f3()
def f5(): return f4()
def f6(): return f5()
def f7(): return f6()
def f8(): return f7()
def f9(): return f8()
def f10(): return f9()
def f11(): return f10()
def g():
    return f11()
# def test_invoke_strict_module_deep_unjitable_many_args(self):
#     codestr = """
#         def f0(): return 42
#         def f1(a, b, c, d, e, f, g, h):
#             class C: pass
#             return f0() - a + b - c + d - e + f - g + h - 4
#         def f2(): return f1(1,2,3,4,5,6,7,8)
#         def f3(): return f2()
#         def f4(): return f3()
#         def f5(): return f4()
#         def f6(): return f5()
#         def f7(): return f6()
#         def f8(): return f7()
#         def f9(): return f8()
#         def f10(): return f9()
#         def f11(): return f10()
#         def g():
#             return f11()
#     """
#     with self.in_strict_module(codestr) as mod:
#         g = mod.g
#         f1 = mod.f1
#         self.assertEqual(g(), 42)
#         self.assertEqual(g(), 42)
#         self.assertInBytecode(
#             g,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "f11"), 0),
#         )
#         self.assert_not_jitted(f1)
