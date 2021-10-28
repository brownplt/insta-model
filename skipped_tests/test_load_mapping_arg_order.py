def test_load_mapping_arg_order(self):
    codestr = """
    def x(a: int, b: int, c: str, d: float=-0.1, e: float=1.1, f: str="something") -> bool:
        return bool(
            a == 1
            and b == 3
            and c == "hi"
            and d == 1.1
            and e == 3.3
            and f == "hmm"
        )
    stuff = []
    def q() -> float:
        stuff.append("q")
        return 1.1
    def r() -> float:
        stuff.append("r")
        return 3.3
    def s() -> str:
        stuff.append("s")
        return "hmm"
    def y() -> bool:
        return x(1, 3, "hi", f=s(), d=q(), e=r())
    """
    y = self.find_code(self.compile(codestr, modname="foo"), name="y")
    self.assertInBytecode(y, "STORE_FAST", "_pystatic_.0._tmp__d")
    self.assertInBytecode(y, "LOAD_FAST", "_pystatic_.0._tmp__d")
    with self.in_module(codestr) as mod:
        y_callable = mod.y
        self.assertTrue(y_callable())
        self.assertEqual(["s", "q", "r"], mod.stuff)
