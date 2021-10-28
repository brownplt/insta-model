def test_load_mapping_arg_custom_class(self):
    """
    Fails because we supply a custom class for the mapped args, instead of a dict
    """
    codestr = """
    def x(a: int, b: int, c: str="hello") -> bool:
        return bool(a == 1 and b == 3 and c == "hello")
    class C:
        def __getitem__(self, key: str) -> str:
            if key == "c":
                return "hi"
        def keys(self):
            return ["c"]
    def y() -> bool:
        return x(1, 3, **C())
    """
    with self.in_module(codestr) as mod:
        y_callable = mod.y
        with self.assertRaisesRegex(
            TypeError, r"argument after \*\* must be a dict, not C"
        ):
            self.assertTrue(y_callable())
