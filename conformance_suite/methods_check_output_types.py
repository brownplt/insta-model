# methods_check_output_types.py
# This should fail.

class C:
    def m(self) -> int: 
        return "foo"
