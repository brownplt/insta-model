# try_except_catch_sub_class.py
# This should pass.
# This should terminate.

class C(Exception):
    pass

def f():
    try:
        raise C("foo")
    except Exception:
        return 42

assert f() is 42