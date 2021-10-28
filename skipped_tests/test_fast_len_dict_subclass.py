def test_fast_len_dict_subclass(self):
    codestr = """
    from typing import Dict
    class mydict(Dict[str, int]):
        def __len__(self):
            return 1111
    def f():
        l = mydict(a=1, b=2)
        return len(l)
    """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertNotInBytecode(f, "FAST_LEN")
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(), 1111)
