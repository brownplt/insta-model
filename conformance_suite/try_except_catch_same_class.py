# try_except_catch_same_class.py
# This should pass.
# This should terminate.

def f():
    try:
        raise Exception("foo")
    except Exception:
        return 42

assert f() is 42