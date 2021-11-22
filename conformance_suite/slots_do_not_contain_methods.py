# slots_do_not_contain_methods.py
# This should pass.
# This should terminate.

class C:
    def mth1(self):
        pass

assert C.__slots__ == ()
