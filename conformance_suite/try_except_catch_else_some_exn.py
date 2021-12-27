# try_except_catch_else_some_exn.py
# This should pass.
# This should terminate.

def f():
    try:
        raise Exception("foo")
    except Exception:
        return 2
    else:
        return 3

assert f() is 2