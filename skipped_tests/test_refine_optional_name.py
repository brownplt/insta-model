def test_refine_optional_name(self):
    codestr = """
    from typing import Optional
    def f(s: Optional[str]) -> bytes:
        return s.encode("utf-8") if s else b""
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f("A"), b"A")
        self.assertEqual(f(None), b"")
