# Reason: Test hitted a banned word int8
def test_array_subscripting_slice(self):
    codestr = """
        from __static__ import Array, int8
        def m() -> Array[int8]:
            a = Array[int8]([1, 3, -5, -1, 7, 22])
            return a[1:3]
    """
    self.compile(codestr, modname="foo")
