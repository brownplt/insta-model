# Reason: Test hitted a banned word int64
def test_field_verifies_type(self):
    codestr = """
    from __static__ import int64
    class C:
        def __init__(self):
            self.x: int64 = 1
    def f():
        return [C().x]
    """
    self.type_error(codestr, "type mismatch: int64 cannot be assigned to dynamic")
