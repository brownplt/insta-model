# redeclare_var_with_def.py
# This should fail.

x: int = 2
def x(): pass