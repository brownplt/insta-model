# for-loop_else_nonbreak.py
# This should pass.
# This should terminate.

for i in [2]:
    pass
else:
    i = 3
assert i is 3