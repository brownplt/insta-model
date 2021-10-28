def test_none_not(self):
    codestr = """
    def t() -> bool:
        x = None
        if not x:
            return True
        else:
            return False
    """
    with self.in_module(codestr) as mod:
        t = mod.t
        self.assertInBytecode(t, "POP_JUMP_IF_TRUE")
        self.assertTrue(t())
