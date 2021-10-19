# test_multiple_dynamic_base_class.py
# This should pass.

from something import A, B
class C(A, B):
    def __init__(self):
        pass