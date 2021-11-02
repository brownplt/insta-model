# redeclare_var_flatten_if.py
# This should fail.

if True:
    x = 2
else:
    x: int = 3