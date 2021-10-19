# test_verify_positional_args_unordered.py
# This should pass.

def x(a: int, b: str) -> None:
    return y(a, b)
def y(a: int, b: str) -> None:
    pass