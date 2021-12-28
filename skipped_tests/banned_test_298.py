# Reason: Test hitted a banned word int8
def test_primitive_return(self):
    cases = [
        ("cbool", True),
        ("cbool", False),
        ("int8", -1 << 7),
        ("int8", (1 << 7) - 1),
        ("int16", -1 << 15),
        ("int16", (1 << 15) - 1),
        ("int32", -1 << 31),
        ("int32", (1 << 31) - 1),
        ("int64", -1 << 63),
        ("int64", (1 << 63) - 1),
        ("uint8", (1 << 8) - 1),
        ("uint8", 0),
        ("uint16", (1 << 16) - 1),
        ("uint16", 0),
        ("uint32", (1 << 32) - 1),
        ("uint32", 0),
        ("uint64", (1 << 64) - 1),
        ("uint64", 0),
    ]
    tf = [True, False]
    for (type, val), box, strict, error, unjitable in itertools.product(
        cases, tf, tf, tf, tf
    ):
        if type == "cbool":
            op = "or"
            other = "False"
            boxed = "bool"
        else:
            op = "*"
            other = "1"
            boxed = "int"
        unjitable_code = "class C: pass" if unjitable else ""
        codestr = f"""
            from __static__ import {type}, box
            def f(error: bool) -> {type}:
                {unjitable_code}
                if error:
                    raise RuntimeError("boom")
                return {val}
        """
        if box:
            codestr += f"""
            def g() -> {boxed}:
                return box(f({error}) {op} {type}({other}))
            """
        else:
            codestr += f"""
            def g() -> {type}:
                return f({error}) {op} {type}({other})
            """
        ctx = self.in_strict_module if strict else self.in_module
        oparg = PRIM_NAME_TO_TYPE[type]
        with self.subTest(
            type=type,
            val=val,
            strict=strict,
            box=box,
            error=error,
            unjitable=unjitable,
        ):
            with ctx(codestr) as mod:
                f = mod.f
                g = mod.g
                self.assertInBytecode(f, "RETURN_PRIMITIVE", oparg)
                if box:
                    self.assertNotInBytecode(g, "RETURN_PRIMITIVE")
                else:
                    self.assertInBytecode(g, "RETURN_PRIMITIVE", oparg)
                if error:
                    with self.assertRaisesRegex(RuntimeError, "boom"):
                        g()
                else:
                    self.assertEqual(g(), val)
                self.assert_jitted(g)
                if unjitable:
                    self.assert_not_jitted(f)
                else:
                    self.assert_jitted(f)
