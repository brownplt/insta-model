# Reason: Test hitted a banned word b'
def test_typed_slots_one_missing(self):
    codestr = """
        class C:
            __slots__ = ('a', 'b')
            __slot_types__ = {'a': (__name__, 'C')}
        inst = C()
    """
    with self.in_module(codestr, code_gen=PythonCodeGenerator) as mod:
        inst, C = mod.inst, mod.C
        self.assertEqual(C.a.__class__.__name__, "typed_descriptor")
        with self.assertRaises(TypeError):
            # type is checked
            inst.a = 42
