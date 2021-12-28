# Reason: Can't be translated by any of the three translator
def test_nonarray_len(self):
    codestr = """
        class Lol:
            def __len__(self):
                return 421
        def y():
            return len(Lol())
    """
    y = self.find_code(self.compile(codestr, modname="foo"), name="y")
    self.assertNotInBytecode(y, "FAST_LEN")
    with self.in_module(codestr) as mod:
        y = mod.y
        self.assertEqual(y(), 421)
