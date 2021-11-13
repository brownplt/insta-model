# Reason: Test hitted some skipped words
def test_float_int_union_error_message(self):
    codestr = """
        class MyFloat(float):
            pass
        def f(x: int) -> int:
            y = 1.0
            if x:
                y = MyFloat("1.5")
            z = x or y
            return z
    """
    self.type_error(codestr, bad_ret_type("float", "int"))
