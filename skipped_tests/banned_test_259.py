# Reason: Test hitted a banned word f"
def test_vector_sizes(self):
    for signed in ["int", "uint"]:
        for size in ["8", "16", "32", "64"]:
            with self.subTest(size=size, signed=signed):
                int_type = f"{signed}{size}"
                codestr = f"""
                    from __static__ import {int_type}, Vector
                    def test() -> Vector[{int_type}]:
                        x: Vector[{int_type}] = Vector[{int_type}]()
                        y: {int_type} = 1
                        x.append(y)
                        return x
                """
                with self.in_module(codestr) as mod:
                    test = mod.test
                    res = test()
                    self.assertEqual(list(res), [1])
