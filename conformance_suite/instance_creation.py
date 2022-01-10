# instance_creation.py
# This should pass.
# This should terminate.

class C:
    def __init__(self, arg) -> None:
        self.x: str = arg

o = C("foo")
assert o.x == "foo"
