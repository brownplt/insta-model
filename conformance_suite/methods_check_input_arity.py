# methods_check_input_arity.py
# This should fail.

class C:
    def m(self, x):
        pass

def f():
    C().m(1, 2)