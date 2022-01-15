# for-loop_else_raise_exit.py
# This should pass.
# This should terminate.

try:
    for i in [2]:
        raise Exception()
    else:
        i = 3
except Exception:
    pass
assert i is 2