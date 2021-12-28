# Reason: Test hitted a banned word int64
def test_array_create_failure(self):
    # todo - in the future we're going to support this, but for now fail it.
    codestr = """
        from __static__ import int64, Array
        class C: pass
        def test() -> Array[C]:
            return Array[C]([1, 3, 5])
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Invalid Array element type: foo.C"
    ):
        self.compile(codestr, modname="foo")
