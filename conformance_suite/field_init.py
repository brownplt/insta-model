# field_init.py
# This should pass.
# This should terminate.

class C:
    x: str
    def __init__(self, arg) -> None:
        self.x = arg

try:
    o = C(42)
except TypeError:
    pass
else:
    raise Exception()
