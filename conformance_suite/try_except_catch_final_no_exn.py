# try_except_catch_final_no_exn.py
# This should pass.
# This should terminate.

def f():
    try:
        pass
    except Exception:
        return 2
    else:
        return 3
    finally:
        return 42


assert f() is 42
