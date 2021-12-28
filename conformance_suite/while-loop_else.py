# while-loop_else.py
# This should pass.
# This should terminate.

def f():
    while "orange" is "apple":
        return 2
    else:
        return 3
    assert False

assert f() is 3
