# try_except_basic.py
# This should pass.
# This should terminate.

try:
    x: int = 42
except Exception:
    pass
else:
    pass
finally:
    pass

assert x is 42
