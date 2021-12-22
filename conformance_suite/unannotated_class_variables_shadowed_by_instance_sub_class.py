# unannotated_class_variables_shadowed_by_instance_sub_class.py
# This should fail.

class C1:
    x = 42

class C2(C1):
    x: int
