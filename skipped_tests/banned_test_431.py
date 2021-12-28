# Reason: Test hitted a banned word int8
def test_primitive_stack_spill(self):
    # Create enough locals that some must get spilled to stack, to test
    # shuffling stack-spilled values across basic block transitions, and
    # field reads/writes with stack-spilled values. These can create
    # mem->mem moves that otherwise wouldn't exist, and trigger issues
    # like push/pop not supporting 8 or 32 bits on x64.
    varnames = string.ascii_lowercase[:20]
    sizes = ["uint8", "int16", "int32", "int64"]
    for size in sizes:
        indent = " " * 20
        attrs = f"\n{indent}".join(f"{var}: {size}" for var in varnames)
        inits = f"\n{indent}".join(
            f"{var}: {size} = {val}" for val, var in enumerate(varnames)
        )
        assigns = f"\n{indent}".join(f"val.{var} = {var}" for var in varnames)
        reads = f"\n{indent}".join(f"{var} = val.{var}" for var in varnames)
        indent = " " * 24
        incrs = f"\n{indent}".join(f"{var} += 1" for var in varnames)
        codestr = f"""
            from __static__ import {size}
            class C:
                {attrs}
            def f(val: C, flag: {size}) -> {size}:
                {inits}
                if flag:
                    {incrs}
                {assigns}
                {reads}
                return {' + '.join(varnames)}
        """
        with self.subTest(size=size):
            with self.in_module(codestr) as mod:
                f, C = mod.f, mod.C
                c = C()
                self.assertEqual(f(c, 0), sum(range(len(varnames))))
                for val, var in enumerate(varnames):
                    self.assertEqual(getattr(c, var), val)
                c = C()
                self.assertEqual(f(c, 1), sum(range(len(varnames) + 1)))
                for val, var in enumerate(varnames):
                    self.assertEqual(getattr(c, var), val + 1)
