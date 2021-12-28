# Reason: Format too complicated
def test_widen_to_dynamic(self):
    self.assertReturns(
        """
        def f(x, flag):
            if flag:
                x = 3
            return x
        """,
        "dynamic",
    )
