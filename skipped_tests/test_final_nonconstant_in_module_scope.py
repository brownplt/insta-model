def test_final_nonconstant_in_module_scope(self):
    codestr = """
    from typing import Final
    def p() -> str:
        return "omg"
    X: Final[str] = p()
    """
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.__final_constants__, ())
