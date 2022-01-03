# for-loop_basic.py
# This should pass.
# This should terminate.

def fact(n: int) -> int:
    o:int = 1
    for i in range(n):
        o *= i + 1
    return o

assert fact(5) is 120
