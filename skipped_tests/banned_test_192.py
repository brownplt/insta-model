# Reason: Test hitted a banned word global
def test_global_uses_decl_type(self):
    codestr = """
        # even though we can locally infer G must be None,
        # it's not Final so nested scopes can't assume it
        # remains None
        G: int | None = None
        def f() -> int:
            global G
            # if we use the local_type for G's type,
            # x would have a local type of None
            x: int | None = G
            if x is None:
                x = G = 1
            return x
    """
    with self.in_strict_module(codestr) as mod:
        self.assertEqual(mod.f(), 1)
