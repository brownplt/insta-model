# iterate-str.py
# This should pass.
# This should terminate.

l = []
for x in "abc":
    l.append(x)

assert l == ["a", "b", "c"]
