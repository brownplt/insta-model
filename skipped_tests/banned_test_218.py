# Reason: Test hitted a banned word int8
def test_vector_presized(self):
    codestr = f"""
        from __static__ import int8, Vector
        def test() -> Vector[int8]:
            x: Vector[int8] = Vector[int8](4)
            x[1] = 1
            return x
    """
    with self.in_module(codestr) as mod:
        f = mod.test
        self.assertEqual(f(), array("b", [0, 1, 0, 0]))
