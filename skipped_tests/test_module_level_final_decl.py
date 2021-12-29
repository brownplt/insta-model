# Reason: Hitted a skipped word (Final)
def test_module_level_final_decl(self):
    codestr = """
    from typing import Final
    x: Final
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Must assign a value when declaring a Final"
    ):
        self.compile(codestr, modname="foo")
