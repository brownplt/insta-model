# Reason: Test hitted a banned word int64
def test_vector_verifies_type(self):
    codestr = """
    from __static__ import int64, Vector
    def f():
        x = Vector[int64]()
        x.append(1)
        return [x[0]]
    """
    self.type_error(codestr, "type mismatch: int64 cannot be assigned to dynamic")
