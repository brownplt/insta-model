# field_update.py
# This should pass.
# This should terminate.

class C:
    def __init__(self, arg) -> None:
        self.x: str = arg

o = C("foo")
try:
    def dyn_int():
        return 42
    o.x = dyn_int()
except TypeError:
    pass
else:
    raise Exception()
