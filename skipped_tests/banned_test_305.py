# Reason: Test hitted a banned word int64
def test_primitive_return_bad_call(self):
    codestr = """
    from __static__ import int64
    def fn(x: int, y: int) -> int64:
        i = int64(x)
        j = int64(y)
        return i + j
    """
    with self.in_module(codestr) as mod:
        fn = mod.fn
        with self.assertRaisesRegex(
            TypeError,
            re.escape("fn() missing 2 required positional arguments: 'x' and 'y'"),
        ):
            fn()  # bad call
