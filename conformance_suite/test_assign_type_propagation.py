# test_assign_type_propagation.py
# This should pass.

def test() -> int:
    x = 5
    return x
# def test_assign_type_propagation(self):
#     codestr = """
#         def test() -> int:
#             x = 5
#             return x
#     """
#     self.compile(codestr, modname="foo")
