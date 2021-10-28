def test_typed_slots_object(self):
    codestr = """
        class C:
            __slots__ = ('a', )
            __slot_types__ = {'a': (__name__, 'C')}
        inst = C()
    """
    with self.in_module(codestr, code_gen=PythonCodeGenerator) as mod:
        inst, C = mod.inst, mod.C
        self.assertEqual(C.a.__class__.__name__, "typed_descriptor")
        with self.assertRaises(TypeError):
            # type is checked
            inst.a = 42
        with self.assertRaises(TypeError):
            inst.a = None
        with self.assertRaises(AttributeError):
            # is initially unassigned
            inst.a
        # can assign correct type
        inst.a = inst
        # __sizeof__ doesn't include GC header size
        self.assertEqual(inst.__sizeof__(), self.base_size + self.ptr_size)
        # size is +2 words for GC header, one word for reference
        self.assertEqual(sys.getsizeof(inst), self.base_size + (self.ptr_size * 3))
        # subclasses are okay
        class D(C):
            pass
        inst.a = D()
