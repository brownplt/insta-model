# Reason: Test hitted some skipped words
def test_typed_slots_private(self):
    codestr = """
        class C:
            __slots__ = ('__a', )
            __slot_types__ = {'__a': (__name__, 'C', '?')}
            def __init__(self):
                self.__a = None
        inst = C()
    """
    with self.in_module(codestr, code_gen=PythonCodeGenerator) as mod:
        inst, C = mod.inst, mod.C
        self.assertEqual(inst._C__a, None)
        inst._C__a = inst
        self.assertEqual(inst._C__a, inst)
        inst._C__a = None
        self.assertEqual(inst._C__a, None)
