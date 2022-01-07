# Reason: Can't be translated by any of the three translator
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
