def test_str_split(self):
    codestr = """
        def get_str() -> str:
            return "something here"
        def test() -> str:
            a, b = get_str().split(None, 1)
            return b
    """
    with self.in_module(codestr) as mod:
        test = mod.test
        self.assertEqual(test(), "here")
