# test_untyped_attr.py
# This should pass.

# We added 2 lines below to declare x.
def f(): return 42
x = f()
y = x.load
x.store = 42
del x.delete
# def test_untyped_attr(self):
#     codestr = """
#     y = x.load
#     x.store = 42
#     del x.delete
#     """
#     code = self.compile(codestr, modname="foo")
#     self.assertInBytecode(code, "LOAD_ATTR", "load")
#     self.assertInBytecode(code, "STORE_ATTR", "store")
#     self.assertInBytecode(code, "DELETE_ATTR", "delete")
