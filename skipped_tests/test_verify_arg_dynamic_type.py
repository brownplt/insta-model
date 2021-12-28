# Reason: Can't be translated by any of the three translator
def test_verify_arg_dynamic_type(self):
    codestr = """
        def x(v:str):
            return 'abc'
        def y(v):
            return x(v)
    """
    module = self.compile(codestr)
    with self.in_module(codestr) as mod:
        y = mod.y
        with self.assertRaises(TypeError):
            y(42)
        self.assertEqual(y("foo"), "abc")
