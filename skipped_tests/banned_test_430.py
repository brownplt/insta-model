# Reason: Test hitted a banned word double
def test_double_binop_with_literal(self):
    codestr = f"""
        from __static__ import double, unbox
        def f():
            y: double = 1.2
            y + 1.0
    """
    f = self.run_code(codestr)["f"]
    f()
