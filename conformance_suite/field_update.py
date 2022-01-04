# field_update.py
# This should pass.
# This should terminate.

class C:
    x: str
    def __init__(self, arg) -> None:
        self.x = arg

o = C("foo")
try:
    def dyn_int():
        return 42
    o.x = dyn_int()
except TypeError:
    pass
else:
    raise Exception()
