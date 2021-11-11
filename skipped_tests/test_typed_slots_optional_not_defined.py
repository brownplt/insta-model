# Reason: Test hitted some skipped words
def test_typed_slots_optional_not_defined(self):
    codestr = """
        class C:
            __slots__ = ('a', )
            __slot_types__ = {'a': (__name__, 'D', '?')}
            def __init__(self):
                self.a = None
        inst = C()
        class D:
            pass
    """
    with self.in_module(codestr, code_gen=PythonCodeGenerator) as mod:
        inst, C = mod.inst, mod.C
        inst.a = None
        self.assertEqual(inst.a, None)
