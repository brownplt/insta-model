def test_final_constant_folding_in_module_scope(self):
    codestr = """
    from typing import Final
    X: Final[int] = 21
    y = X + 3
    """
    c = self.compile(codestr, modname="foo.py")
    self.assertNotInBytecode(c, "LOAD_NAME", "X")
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.y, 24)
