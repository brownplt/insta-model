# methods_check_input_types.py
# This should fail.

class C:
    def m(self, x: int):
        return

C().m("foo")