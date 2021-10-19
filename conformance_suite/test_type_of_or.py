# test_type_of_or.py
# This should pass.

def f(x: int, y: str) -> int | str:
    return x or y