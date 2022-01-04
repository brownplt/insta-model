# test_invoke_strict_module_deep.py
# This should pass.
# This should terminate.

def f0(): return 42
def f1(): return f0()
def f2(): return f1()
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
# def test_invoke_strict_module_deep(self):
#     codestr = """
#         def f0(): return 42
#         def f1(): return f0()
#         def f2(): return f1()
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
#         self.assertEqual(g(), 42)
#         self.assertEqual(g(), 42)
#         self.assertInBytecode(g, "INVOKE_FUNCTION", ((mod.__name__, "f11"), 0))
