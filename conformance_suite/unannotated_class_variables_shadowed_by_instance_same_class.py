# unannotated_class_variables_shadowed_by_instance_same_class.py
# This should fail.

class C1:
    x = 42
    x: int