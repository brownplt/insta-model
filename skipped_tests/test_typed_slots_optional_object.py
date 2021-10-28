def test_typed_slots_optional_object(self):
    codestr = """
        class C:
            __slots__ = ('a', )
            __slot_types__ = {'a': (__name__, 'C', '?')}
        inst = C()
    """
    with self.in_module(codestr, code_gen=PythonCodeGenerator) as mod:
        inst, C = mod.inst, mod.C
        inst.a = None
        self.assertEqual(inst.a, None)
