#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(require "type-check.rkt")



(check-judgment-holds*
 (⊢p ())
 (⊢p (pass))
 (⊢p (pass
      pass))
 (⊢p (42))
 (⊢p (#t))
 (⊢p (#f))
 (⊢p ((define x int 42)))
 (⊢p ((define x int 42)
      x))
 (⊢p ((class C object)))
 (⊢p ((class C object
        (field x int)
        (method y self ((arg int)) int
                (return 42))))))

#|
The goal is to create a test suite for our model. At this stage, let's focus on static errors.
|#

#|
    def test_incompat_override(self):
        codestr = """
        class C:
            x: int

        class D(C):
            def x(self): pass
        """
        with self.assertRaises(TypedSyntaxError):
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
|#


(check-not-judgment-holds*
 (⊢p ((class C object
        (field x int))
      (class D C
        (method x self () dynamic
                pass)))))


(check-not-judgment-holds*
 (⊢p ((class A object
        (method m self () str
                (return "hello")))
      (class B A
        (method m self () int
                (return 0))))))


(check-judgment-holds*
 (⊢p ((class C object
        (field x int))
      (class D C
        (field x int))))
 (⊢p ((class C object
        (field x int))
      (class D C
        (field x str)))))