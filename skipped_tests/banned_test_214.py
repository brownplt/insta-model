# Reason: Test hitted a banned word int64
def test_vector_import(self):
    codestr = """
        from __static__ import int64, Vector
        def test() -> Vector[int64]:
            x: Vector[int64] = Vector[int64]()
            x.append(1)
            return x
    """
    with self.in_module(codestr) as mod:
        test = mod.test
        self.assertEqual(test(), array("L", [1]))
