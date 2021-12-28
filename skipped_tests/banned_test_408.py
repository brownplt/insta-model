# Reason: Test hitted a banned word int64
def test_dynamic_type_param(self):
    """DYNAMIC as type param of generic doesn't render the whole type DYNAMIC."""
    codestr = """
        from __static__ import int64, clen
        from nonstatic import Foo
        from typing import Dict
        def f(d: Dict[Foo, int]):
            x: int64 = clen(d)
    """
    self.compile(codestr)
