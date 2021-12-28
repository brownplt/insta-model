# Reason: Test hitted a banned word box
def test_tuple_assign_list(self):
    codestr = """
        from __static__ import int16, box
        def testfunc(a: int, b: int):
            x: int
            y: str
            x, y = [a, b]
    """
    with self.assertRaisesRegex(TypedSyntaxError, "int cannot be assigned to str"):
        self.compile(codestr, modname="foo")
