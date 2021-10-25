# test_verify_positional_args.py
# This should fail.

def x(a: int, b: str) -> None:
    pass
x("a", 2)