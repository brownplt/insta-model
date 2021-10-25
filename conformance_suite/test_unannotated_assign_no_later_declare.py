# test_unannotated_assign_no_later_declare.py
# This should fail.

def f(flag):
    x = None
    if flag:
        x: str = "foo"