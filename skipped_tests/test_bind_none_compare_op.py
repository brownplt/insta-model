def test_bind_none_compare_op(self):
    codestr = """
        from typing import Any
        def has_none(x) -> bool:
            return None in x
        def has_no_none(x) -> bool:
            return None not in x
    """
    with self.in_module(codestr) as mod:
        has_none = mod.has_none
        self.assertFalse(has_none([]))
        self.assertTrue(has_none([None]))
        self.assertFalse(has_none([1, 2, 3]))
        self.assertNotInBytecode(has_none, "CAST")
        has_no_none = mod.has_no_none
        self.assertTrue(has_no_none([]))
        self.assertFalse(has_no_none([None]))
        self.assertTrue(has_no_none([1, 2, 3]))
        self.assertNotInBytecode(has_no_none, "CAST")
