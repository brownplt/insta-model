# test_assign_to_object.py
# This should pass.

def f():
    x: object
    x = None
    x = 1
    x = 'abc'
    x = {}
    x = ()
    x = 1.0
    x = int
    x = True
