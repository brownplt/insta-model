def test_mixed_chain_assign(self) -> None:
    codestr = """
        class C: pass
        class D: pass
        def f():
            x: C = C()
            y: D = D()
            x = y = D()
    """
    with self.assertRaisesRegex(TypedSyntaxError, type_mismatch("foo.D", "foo.C")):
        self.compile(codestr, modname="foo")
