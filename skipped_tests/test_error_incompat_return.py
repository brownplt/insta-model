def test_error_incompat_return(self):
    with self.assertRaises(TypedSyntaxError):
        code = self.compile(
            """
            class D: pass
            class C:
                def __init__(self):
                    self.x = None
                def f(self) -> "C":
                    return D()
            """
        )
