# Reason: Can't be translated by any of the three translator
def test_compile_method(self):
    code = self.compile(
        """
        from __static__ import ssize_t
        def f():
            x: ssize_t = 42
        """
    )
    f = self.find_code(code)
    self.assertInBytecode(f, "PRIMITIVE_LOAD_CONST", (42, TYPED_INT64))
