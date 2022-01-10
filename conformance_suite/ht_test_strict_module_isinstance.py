# test_strict_module_isinstance.py
# This should pass.

# This whole test is translated manually because our script
#   can't handle this format.

from typing import Optional
def foo(tval: Optional[object]) -> str:
    if isinstance(tval, str):
        return tval
    return "hi"

# def test_strict_module_isinstance(self):
#     code = """
#         from typing import Optional
#         def foo(tval: Optional[object]) -> str:
#             if isinstance(tval, str):
#                 return tval
#             return "hi"
#     """
#     self.compile_strict(code)
