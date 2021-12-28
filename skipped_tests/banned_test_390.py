# Reason: Test hitted a banned word box
def test_posix_clock_gettime_ns(self):
    codestr = """
    from __static__ import box, posix_clock_gettime_ns
    def test() -> int:
        x = posix_clock_gettime_ns()
        return box(x)
    """
    with self.in_module(codestr) as mod:
        test = mod.test
        expected = time.clock_gettime_ns(time.CLOCK_MONOTONIC)
        res = test()
        ten_sec_in_nanosec = 1e10
        self.assertEqual(type(res), int)
        # It is pretty reasonable to expect this test to finish within +/- 10 sec
        self.assertTrue(
            expected - ten_sec_in_nanosec <= res <= expected + ten_sec_in_nanosec
        )
