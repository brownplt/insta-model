# Reason: Format too complicated
def test_load_mapping_arg(self):
    codestr = """
    def x(a: int, b: int, c: str, d: float=-0.1, e: float=1.1, f: str="something") -> bool:
        return bool(f == "yo" and d == 1.0 and e == 1.1)
    def y() -> bool:
        d = {"d": 1.0}
        return x(1, 3, "hi", f="yo", **d)
    """
    y = self.find_code(self.compile(codestr, modname="foo"), name="y")
    self.assertInBytecode(y, "LOAD_MAPPING_ARG", 3)
    with self.in_module(codestr) as mod:
        y_callable = mod.y
        self.assertTrue(y_callable())
