# Reason: Test hitted a banned word global
def test_assign_module_global(self):
    codestr = """
        x: int = 1
        def f():
            global x
            x = "foo"
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("Exact[str]", "int")
    ):
        self.compile(codestr)
