# test_module_subclass.py
# This should pass.
# This should terminate.

class C:
    def __init__(self):
        self.x: Optional[C] = None
# def test_module_subclass(self):
#     codestr = """
#     class C:
#         def __init__(self):
#             self.x: Optional[C] = None
#     """
#     with self.in_module(codestr) as mod:
#         C = mod.C
#         class CustomModule(ModuleType):
#             def __getattr__(self, name):
#                 if name == "C":
#                     return C
#         sys.modules[mod.__name__] = CustomModule(mod.__name__)
#         c = C()
#         self.assertEqual(c.x, None)
