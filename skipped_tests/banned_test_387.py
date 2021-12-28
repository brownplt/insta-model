# Reason: Test hitted a banned word int64
def test_rand(self):
    codestr = """
    from __static__ import rand, RAND_MAX, box, int64
    def test():
        x: int64 = rand()
        return box(x)
    """
    with self.in_module(codestr) as mod:
        test = mod.test
        self.assertEqual(type(test()), int)
