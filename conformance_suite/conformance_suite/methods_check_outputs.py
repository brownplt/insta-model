# methods_check_outputs.py
# This should fail.

class C:
    def m(self) -> int: 
        return "foo"
