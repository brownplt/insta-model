# Reason: Test hitted a banned word float
def test_chkdict_float_is_dynamic(self):
    codestr = """
    from __static__ import CheckedDict
    def main():
        d = CheckedDict[float, str]({2.0: "hello", 2.3: "foobar"})
        reveal_type(d)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"reveal_type\(d\): 'Exact\[chkdict\[dynamic, str\]\]'",
    ):
        self.compile(codestr)
