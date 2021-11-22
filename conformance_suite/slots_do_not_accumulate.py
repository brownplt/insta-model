# slots_do_not_accumulate.py
# This should pass.
# This should terminate.

class C1:
    x: str

class C2(C1):
    y: int

assert C2.__slots__ == ('y',)
