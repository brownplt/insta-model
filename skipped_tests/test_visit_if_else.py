def test_visit_if_else(self):
    codestr = """
        x = 0
        if x:
            pass
        else:
            def f(): return 42
    """
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.f(), 42)
