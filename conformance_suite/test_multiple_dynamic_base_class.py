# test_multiple_dynamic_base_class.py
# This should pass.

from something import A, B
class C(A, B):
    def __init__(self):
        pass
# def test_multiple_dynamic_base_class(self) -> None:
#     codestr = """
#     from something import A, B
#     class C(A, B):
#         def __init__(self):
#             pass
#     """
#     self.compile(codestr)
