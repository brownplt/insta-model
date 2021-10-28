def test_load_uninit_module(self):
    """verify we don't crash if we receive a module w/o a dictionary"""
    codestr = """
    class C:
        def __init__(self):
            self.x: Optional[C] = None
    """
    with self.in_module(codestr) as mod:
        C = mod.C
        class UninitModule(ModuleType):
            def __init__(self):
                # don't call super init
                pass
        sys.modules[mod.__name__] = UninitModule()
        with self.assertRaisesRegex(
            TypeError,
            r"bad name provided for class loader: \('"
            + mod.__name__
            + r"', 'C'\), not a class",
        ):
            C()
