# field_init.py
# This should pass.
# This should terminate.

class C:
    def __init__(self, arg) -> None:
        self.x: str = arg

try:
    o = C(42)
except TypeError:
    pass
else:
    raise Exception()
