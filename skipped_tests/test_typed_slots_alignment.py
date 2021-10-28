def test_typed_slots_alignment(self):
    return
    codestr = """
        class C:
            __slots__ = ('a', 'b')
            __slot_types__ {'a': ('__static__', 'int16')}
        inst = C()
    """
    with self.in_module(codestr, code_gen=PythonCodeGenerator) as mod:
        inst, C = mod.inst, mod.C
        inst.a = None
        self.assertEqual(inst.a, None)
