# methods_check_inputs.py
# This should fail.

class C:
    def m(self, x: int):
        return

C().m("foo")