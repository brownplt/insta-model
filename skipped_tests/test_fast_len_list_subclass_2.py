def test_fast_len_list_subclass_2(self):
    codestr = """
    class mylist(list):
        def __len__(self):
            return 1111
    def f(x):
        l = [1, 2]
        if x:
            l = mylist([1, 2])
        return len(l)
    """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "FAST_LEN", FAST_LEN_LIST | FAST_LEN_INEXACT)
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(True), 1111)
