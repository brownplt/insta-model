# instance_creation.py
# This should pass.
# This should terminate.

class C:
    x: str
    def __init__(self, arg) -> None:
        self.x = arg

o = C("foo")
assert o.x == "foo"
