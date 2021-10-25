# test_duplicate_function_replaces_class.py
# This should fail.

class X: pass
def X(): pass