def test_compile_nested_class(self):
    codestr = """
        from typing import ClassVar
        class Outer:
            class Inner:
                c: ClassVar[int] = 1
    """
    self.compile(codestr)
    codestr = """
        from typing import ClassVar
        class Outer:
            class Inner1:
                c: ClassVar[int] = 1
                class Inner2:
                    c: ClassVar[int] = 2
                    class Inner3:
                        c: ClassVar[int] = 3
    """
    self.compile(codestr)
