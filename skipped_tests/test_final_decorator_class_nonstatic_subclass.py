def test_final_decorator_class_nonstatic_subclass(self):
    codestr = """
        from typing import final
        @final
        class C:
            pass
    """
    with self.in_module(codestr) as mod:
        with self.assertRaisesRegex(
            TypeError, "type 'C' is not an acceptable base type"
        ):
            class D(mod.C):
                pass
