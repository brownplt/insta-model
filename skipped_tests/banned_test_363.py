# Reason: Test hitted a banned word float
def test_primitive_double_return_bad_call(self):
    codestr = """
    from __static__ import double
    def fn(x: float, y: float) -> double:
        i = double(x)
        j = double(y)
        return i + j
    """
    with self.in_module(codestr) as mod:
        fn = mod.fn
        with self.assertRaisesRegex(
            TypeError,
            re.escape("fn() missing 2 required positional arguments: 'x' and 'y'"),
        ):
            fn()  # bad call
