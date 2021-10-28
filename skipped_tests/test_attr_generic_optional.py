def test_attr_generic_optional(self):
    codestr = """
        from typing import Optional
        def f(x: Optional):
            return x.foo
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "cannot access attribute from unbound Union"
    ):
        self.compile(codestr, modname="foo")
