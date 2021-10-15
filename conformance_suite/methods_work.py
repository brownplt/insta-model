# methods_work.py
# This should pass.

class C:
    def m(self, x: int) -> str:
        return "foo"

s: str = C().m(42)
