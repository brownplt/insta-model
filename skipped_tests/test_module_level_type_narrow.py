# Reason: Hitted a skipped word (reveal_type)
def test_module_level_type_narrow(self):
    codestr = """
        def a() -> int | None:
            return 1
        G = a()
        if G is not None:
            G += 1
        def f() -> int:
            if G is None:
                return 0
            reveal_type(G)
    """
    with self.assertRaisesRegex(TypedSyntaxError, r"Optional\[int\]"):
        self.compile(codestr)
