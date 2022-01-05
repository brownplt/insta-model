# test_module_subclass.py
# This should pass.
# This should terminate.

# The following line is added because this test should have imported
#   Optional.
from typing import Optional
class C:
    # The following line is added because we moved the annotation
    #   here from somewhere else.
    x: Optional[C]
    def __init__(self):
        # The following line is edited to move the annotation outward
        # self.x: Optional[C] = None
        self.x = None
c = C()
assert c.x == None

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
