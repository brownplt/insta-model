# Reason: Test hitted a banned word global
def test_inferred_module_global_assign_subclass(self):
    codestr = """
        class MyList(list):
            pass
        x = []
        def f(new_x: list) -> list:
            global x
            x = new_x
            return x
    """
    with self.in_module(codestr) as mod:
        f, MyList = mod.f, mod.MyList
        x = []
        self.assertIs(f(x), x)
        y = MyList()
        self.assertIs(f(y), y)
