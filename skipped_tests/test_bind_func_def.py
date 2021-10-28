def test_bind_func_def(self) -> None:
    mod, comp = self.bind_module(
        """
        def f(x: object = None, y: object = None):
            pass
    """
    )
    modtable = comp.modules["foo"]
    self.assertTrue(isinstance(modtable.children["f"], Function))
