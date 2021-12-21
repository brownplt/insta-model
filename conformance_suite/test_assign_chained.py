# test_assign_chained.py
# This should pass.

def test() -> str:
    x: str = "hi"
    y = x = "hello"
    return y
# def test_assign_chained(self):
#     codestr = """
#         def test() -> str:
#             x: str = "hi"
#             y = x = "hello"
#             return y
#     """
#     self.compile(codestr, modname="foo")
