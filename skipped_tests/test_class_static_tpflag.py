def test_class_static_tpflag(self):
    codestr = """
    class A:
        pass
    """
    with self.in_module(codestr) as mod:
        A = mod.A
        self.assertTrue(is_type_static(A))
        class B:
            pass
        self.assertFalse(is_type_static(B))
