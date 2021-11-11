# Reason: Format too complicated
def test_fast_len_dict_subclass_2(self):
    codestr = """
    from typing import Dict
    class mydict(Dict[str, int]):
        def __len__(self):
            return 1111
    def f(x, a, b):
        l: Dict[str, int] = {'c': 3}
        if x:
            l = mydict(a=1, b=2)
        return len(l)
    """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "FAST_LEN", FAST_LEN_DICT | FAST_LEN_INEXACT)
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(True, 1, 2), 1111)
