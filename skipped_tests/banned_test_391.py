# Reason: Test hitted a banned word f"
def test_primitive_types_final(self):
    PRIMITIVE_TYPES = ALL_CINT_TYPES + [CBOOL_TYPE, CHAR_TYPE, DOUBLE_TYPE]
    PRIMITIVE_NAMES = [klass.instance_name for klass in PRIMITIVE_TYPES]
    for name in PRIMITIVE_NAMES:
        codestr = f"""
            from __static__ import {name}
            class C({name}): pass
        """
        with self.subTest(klass=name):
            self.type_error(
                codestr,
                f"Primitive type {name} cannot be subclassed: ",
                at="class C",
            )
