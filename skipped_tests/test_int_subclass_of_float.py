# Reason: Test hitted some skipped words
def test_int_subclass_of_float(self):
    """PEP 484 specifies that ints should be treated as subclasses of floats,
    even though they differ in the runtime."""
    codestr = """
        def takes_float(f: float) -> float:
            return f
        a: int = 1
        x: float = takes_float(a)
    """
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.x, 1)
