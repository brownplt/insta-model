def test_assign_generic_optional(self):
    codestr = """
        from typing import Optional
        def f():
            x: Optional = 42
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("Literal[42]", "Optional[T]")
    ):
        self.compile(codestr, modname="foo")
