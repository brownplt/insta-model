#lang racket
(require redex)
(require "../core.scm")

#|
The goal is to create a test suite for our model. At this stage, let's focus on static errors.
|#

#|
    def test_reveal_type_local(self) -> None:
        codestr = """
            def f(x: int | None):
                if x is not None:
                    reveal_type(x)
        """
        with self.assertRaisesRegex(
            TypedSyntaxError,
            r"reveal_type\(x\): 'int', 'x' has declared type 'Optional\[int\]' and local type 'int'",
        ):
            self.compile(codestr)

    def test_incompat_override(self):
        codestr = """
        class C:
            x: int

        class D(C):
            def x(self): pass
        """
        with self.assertRaises(TypedSyntaxError):
            self.compile(codestr, StaticCodeGenerator, modname="foo")

    def test_final_reassigned_in_loop(self):
        codestr = """
        from typing import Final

        x: Final[int] = 0xdeadbeef

        for x in [1, 3, 5]:
            pass
        """
        with self.assertRaisesRegex(
            TypedSyntaxError, "Cannot assign to a Final variable"
        ):
            self.compile(codestr, StaticCodeGenerator, modname="foo")

    def test_compile_dict_setdefault(self):
        codestr = """
            from __static__ import CheckedDict
            def testfunc():
                x = CheckedDict[int, str]({42: 'abc', })
                x.setdefault(100, 43)
        """
        with self.assertRaisesRegex(
            TypedSyntaxError,
            r"Literal\[43\] received for positional arg 2, expected Optional\[str\]",
        ):
            self.compile(codestr, StaticCodeGenerator, modname="foo")

    def test_class_level_final_reinitialized_in_subclass_init(self):
        codestr = """
        from typing import Final

        class C:
            x: Final[int] = 3

        class D(C):
            def __init__(self):
                self.x = 4
        """
        with self.assertRaisesRegex(
            TypedSyntaxError,
            "Cannot assign to a Final attribute of foo.D:x",
        ):
            self.compile(codestr, StaticCodeGenerator, modname="foo")

|#