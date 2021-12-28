# while-loop_basic.py
# This should pass.
# This should terminate.

def fact(i: int) -> int:
    o:int = 1
    while i > 0:
        o *= i
        i -= 1
    return o

assert fact(5) is 120
