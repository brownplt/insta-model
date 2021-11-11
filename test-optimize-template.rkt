#lang racket
(require redex)
(require redex-abbrevs)
(require "desugar.rkt")
(require "compile.rkt")

(define-language Matcher
  (Any hole
  (any ... Any any ...)))

;; conformance_suite/test_compile_dict_setitem.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (1 "abc")))) (expr ((attribute x "__setitem__") 2 "def")) (return x))))))))
#|

from __static__ import CheckedDict
def testfunc():
    x = CheckedDict[int, str]({1:'abc'})
    x.__setitem__(2, 'def')
    return x
# def test_compile_dict_setitem(self):
#     codestr = """
#         from __static__ import CheckedDict
#         def testfunc():
#             x = CheckedDict[int, str]({1:'abc'})
#             x.__setitem__(2, 'def')
#             return x
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         x = test()
#         self.assertInBytecode(
#             test,
#             "INVOKE_FUNCTION",
#             (
#                 (
#                     "__static__",
#                     "chkdict",
#                     (("builtins", "int"), ("builtins", "str")),
#                     "__setitem__",
#                 ),
#                 3,
#             ),
#         )
#         self.assertEqual(x, {1: "abc", 2: "def"})
|#


;; conformance_suite/test_generic_method_ret_type.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (define/assign MAP (subscript CheckedDict (tuple-syntax str (subscript Optional str))) ((subscript CheckedDict (tuple-syntax str (subscript Optional str))) (dict-syntax ("abc" "foo") ("bar" None)))) (def f ((x str)) (subscript Optional str) (begin (return ((attribute MAP "get") x)))))))))
#|

from __static__ import CheckedDict
from typing import Optional
MAP: CheckedDict[str, Optional[str]] = CheckedDict[str, Optional[str]]({'abc': 'foo', 'bar': None})
def f(x: str) -> Optional[str]:
    return MAP.get(x)
# def test_generic_method_ret_type(self):
#     codestr = """
#         from __static__ import CheckedDict
#         from typing import Optional
#         MAP: CheckedDict[str, Optional[str]] = CheckedDict[str, Optional[str]]({'abc': 'foo', 'bar': None})
#         def f(x: str) -> Optional[str]:
#             return MAP.get(x)
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(
#             f,
#             "INVOKE_FUNCTION",
#             (
#                 (
#                     "__static__",
#                     "chkdict",
#                     (("builtins", "str"), ("builtins", "str", "?")),
#                     "get",
#                 ),
#                 3,
#             ),
#         )
#         self.assertEqual(f("abc"), "foo")
#         self.assertEqual(f("bar"), None)
|#


;; conformance_suite/test_invoke_all_extra_args.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def target ((a dynamic) (b dynamic) (c dynamic) (d dynamic) (e dynamic) (f dynamic) (g dynamic)) dynamic (begin (return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * a 2) (bin-op * b 3)) (bin-op * c 4)) (bin-op * d 5)) (bin-op * e 6)) (bin-op * f 7)) g)))) (def testfunc () dynamic (begin (return (target 1 2 3 4 5 6 7)))))))))
#|

def target(a, b, c, d, e, f, g):
    return a * 2 + b * 3 + c * 4 + d * 5 + e * 6 + f * 7 + g
def testfunc():
    return target(1,2,3,4,5,6,7)
# def test_invoke_all_extra_args(self):
#     codestr = """
#         def target(a, b, c, d, e, f, g):
#             return a * 2 + b * 3 + c * 4 + d * 5 + e * 6 + f * 7 + g
#         def testfunc():
#             return target(1,2,3,4,5,6,7)
#     """
#     with self.in_strict_module(codestr) as mod:
#         f = mod.testfunc
#         self.assertInBytecode(
#             f,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "target"), 7),
#         )
#         self.assertEqual(f(), 119)
|#


;; conformance_suite/test_invoke_all_reg_args.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def target ((a dynamic) (b dynamic) (c dynamic) (d dynamic) (e dynamic) (f dynamic)) dynamic (begin (return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * a 2) (bin-op * b 3)) (bin-op * c 4)) (bin-op * d 5)) (bin-op * e 6)) (bin-op * f 7))))) (def testfunc () dynamic (begin (return (target 1 2 3 4 5 6)))))))))
#|

def target(a, b, c, d, e, f):
    return a * 2 + b * 3 + c * 4 + d * 5 + e * 6 + f * 7
def testfunc():
    return target(1,2,3,4,5,6)
# def test_invoke_all_reg_args(self):
#     codestr = """
#         def target(a, b, c, d, e, f):
#             return a * 2 + b * 3 + c * 4 + d * 5 + e * 6 + f * 7
#         def testfunc():
#             return target(1,2,3,4,5,6)
#     """
#     with self.in_strict_module(codestr) as mod:
#         f = mod.testfunc
#         self.assertInBytecode(
#             f,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "target"), 6),
#         )
#         self.assertEqual(f(), 112)
|#


;; conformance_suite/test_invoke_chkdict_method.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (def dict--maker () (subscript CheckedDict (tuple-syntax int int)) (begin (return ((subscript CheckedDict (tuple-syntax int int)) (dict-syntax (2 2)))))) (def func () dynamic (begin (define/assign a dynamic (dict--maker)) (return ((attribute a "keys"))))))))))
#|

from __static__ import CheckedDict
def dict_maker() -> CheckedDict[int, int]:
    return CheckedDict[int, int]({2:2})
def func():
    a = dict_maker()
    return a.keys()
# def test_invoke_chkdict_method(self):
#     codestr = """
#     from __static__ import CheckedDict
#     def dict_maker() -> CheckedDict[int, int]:
#         return CheckedDict[int, int]({2:2})
#     def func():
#         a = dict_maker()
#         return a.keys()
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.func
#         self.assertInBytecode(
#             f,
#             "INVOKE_FUNCTION",
#             (
#                 (
#                     "__static__",
#                     "chkdict",
#                     (("builtins", "int"), ("builtins", "int")),
#                     "keys",
#                 ),
#                 1,
#             ),
#         )
#         self.assertEqual(list(f()), [2])
#         self.assert_jitted(f)
|#


;; conformance_suite/test_invoke_int_method.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def func () dynamic (begin (define/assign a dynamic 42) (return ((attribute a "bit_length"))))))))))
#|

def func():
    a = 42
    return a.bit_length()
# def test_invoke_int_method(self):
#     codestr = """
#     def func():
#         a = 42
#         return a.bit_length()
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.func
#         self.assertInBytecode(
#             f, "INVOKE_FUNCTION", (("builtins", "int", "bit_length"), 1)
#         )
#         self.assertEqual(f(), 6)
|#


;; conformance_suite/test_invoke_str_method.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def func () dynamic (begin (define/assign a dynamic "a b c") (return ((attribute a "split"))))))))))
#|

def func():
    a = 'a b c'
    return a.split()
# def test_invoke_str_method(self):
#     codestr = """
#     def func():
#         a = 'a b c'
#         return a.split()
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.func
#         self.assertInBytecode(
#             f, "INVOKE_FUNCTION", (("builtins", "str", "split"), 1)
#         )
#         self.assertEqual(f(), ["a", "b", "c"])
|#


;; conformance_suite/test_invoke_str_method_arg.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def func () dynamic (begin (define/assign a dynamic "a b c") (return ((attribute a "split") "a")))))))))
#|

def func():
    a = 'a b c'
    return a.split('a')
# def test_invoke_str_method_arg(self):
#     codestr = """
#     def func():
#         a = 'a b c'
#         return a.split('a')
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.func
#         self.assertInBytecode(
#             f, "INVOKE_FUNCTION", (("builtins", "str", "split"), 2)
#         )
#         self.assertEqual(f(), ["", " b c"])
|#


;; conformance_suite/test_invoke_strict_module_deep.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def f0 () dynamic (begin (return 42))) (def f1 () dynamic (begin (return (f0)))) (def f2 () dynamic (begin (return (f1)))) (def f3 () dynamic (begin (return (f2)))) (def f4 () dynamic (begin (return (f3)))) (def f5 () dynamic (begin (return (f4)))) (def f6 () dynamic (begin (return (f5)))) (def f7 () dynamic (begin (return (f6)))) (def f8 () dynamic (begin (return (f7)))) (def f9 () dynamic (begin (return (f8)))) (def f10 () dynamic (begin (return (f9)))) (def f11 () dynamic (begin (return (f10)))) (def g () dynamic (begin (return (f11)))))))))
#|

def f0(): return 42
def f1(): return f0()
def f2(): return f1()
def f3(): return f2()
def f4(): return f3()
def f5(): return f4()
def f6(): return f5()
def f7(): return f6()
def f8(): return f7()
def f9(): return f8()
def f10(): return f9()
def f11(): return f10()
def g():
    return f11()
# def test_invoke_strict_module_deep(self):
#     codestr = """
#         def f0(): return 42
#         def f1(): return f0()
#         def f2(): return f1()
#         def f3(): return f2()
#         def f4(): return f3()
#         def f5(): return f4()
#         def f6(): return f5()
#         def f7(): return f6()
#         def f8(): return f7()
#         def f9(): return f8()
#         def f10(): return f9()
#         def f11(): return f10()
#         def g():
#             return f11()
#     """
#     with self.in_strict_module(codestr) as mod:
#         g = mod.g
#         self.assertEqual(g(), 42)
#         self.assertEqual(g(), 42)
#         self.assertInBytecode(g, "INVOKE_FUNCTION", ((mod.__name__, "f11"), 0))
|#


;; conformance_suite/test_invoke_strict_module_deep_unjitable_many_args.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def f0 () dynamic (begin (return 42))) (def f1 ((a dynamic) (b dynamic) (c dynamic) (d dynamic) (e dynamic) (f dynamic) (g dynamic) (h dynamic)) dynamic (begin (class C (object)) (return (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (f0) a) b) c) d) e) f) g) h) 4)))) (def f2 () dynamic (begin (return (f1 1 2 3 4 5 6 7 8)))) (def f3 () dynamic (begin (return (f2)))) (def f4 () dynamic (begin (return (f3)))) (def f5 () dynamic (begin (return (f4)))) (def f6 () dynamic (begin (return (f5)))) (def f7 () dynamic (begin (return (f6)))) (def f8 () dynamic (begin (return (f7)))) (def f9 () dynamic (begin (return (f8)))) (def f10 () dynamic (begin (return (f9)))) (def f11 () dynamic (begin (return (f10)))) (def g () dynamic (begin (return (f11)))))))))
#|

def f0(): return 42
def f1(a, b, c, d, e, f, g, h):
    class C: pass
    return f0() - a + b - c + d - e + f - g + h - 4
def f2(): return f1(1,2,3,4,5,6,7,8)
def f3(): return f2()
def f4(): return f3()
def f5(): return f4()
def f6(): return f5()
def f7(): return f6()
def f8(): return f7()
def f9(): return f8()
def f10(): return f9()
def f11(): return f10()
def g():
    return f11()
# def test_invoke_strict_module_deep_unjitable_many_args(self):
#     codestr = """
#         def f0(): return 42
#         def f1(a, b, c, d, e, f, g, h):
#             class C: pass
#             return f0() - a + b - c + d - e + f - g + h - 4
#         def f2(): return f1(1,2,3,4,5,6,7,8)
#         def f3(): return f2()
#         def f4(): return f3()
#         def f5(): return f4()
#         def f6(): return f5()
#         def f7(): return f6()
#         def f8(): return f7()
#         def f9(): return f8()
#         def f10(): return f9()
#         def f11(): return f10()
#         def g():
#             return f11()
#     """
#     with self.in_strict_module(codestr) as mod:
#         g = mod.g
#         f1 = mod.f1
#         self.assertEqual(g(), 42)
#         self.assertEqual(g(), 42)
#         self.assertInBytecode(
#             g,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "f11"), 0),
#         )
#         self.assert_not_jitted(f1)
|#


;; conformance_suite/test_invoke_strict_module_mutual_recursive.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def fib1 ((number dynamic)) dynamic (begin (if (<= number 1) (begin (return number)) (begin)) (return (bin-op + (fib (bin-op - number 1)) (fib (bin-op - number 2)))))) (def fib ((number dynamic)) dynamic (begin (if (<= number 1) (begin (return number)) (begin)) (return (bin-op + (fib1 (bin-op - number 1)) (fib1 (bin-op - number 2)))))))))))
#|

def fib1(number):
    if number <= 1:
        return number
    return(fib(number-1) + fib(number-2))
def fib(number):
    if number <= 1:
        return number
    return(fib1(number-1) + fib1(number-2))
# def test_invoke_strict_module_mutual_recursive(self):
#     codestr = """
#         def fib1(number):
#             if number <= 1:
#                 return number
#             return(fib(number-1) + fib(number-2))
#         def fib(number):
#             if number <= 1:
#                 return number
#             return(fib1(number-1) + fib1(number-2))
#     """
#     with self.in_strict_module(codestr) as mod:
#         fib = mod.fib
#         fib1 = mod.fib1
#         self.assertInBytecode(
#             fib,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "fib1"), 1),
#         )
#         self.assertInBytecode(
#             fib1,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "fib"), 1),
#         )
#         self.assertEqual(fib(0), 0)
#         self.assert_jitted(fib1)
#         self.assertEqual(fib(4), 3)
|#


;; conformance_suite/test_invoke_strict_module_pre_invoked.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def f () dynamic (begin (return 42))) (def g () dynamic (begin (return (f)))))))))
#|

def f():
    return 42
def g():
    return f()
# def test_invoke_strict_module_pre_invoked(self):
#     codestr = """
#         def f():
#             return 42
#         def g():
#             return f()
#     """
#     with self.in_strict_module(codestr) as mod:
#         self.assertEqual(mod.f(), 42)
#         self.assert_jitted(mod.f)
#         g = mod.g
#         self.assertEqual(g(), 42)
#         self.assertInBytecode(
#             g,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "f"), 0),
#         )
|#


;; conformance_suite/test_invoke_strict_module_recursive.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def fib ((number dynamic)) dynamic (begin (if (<= number 1) (begin (return number)) (begin)) (return (bin-op + (fib (bin-op - number 1)) (fib (bin-op - number 2)))))))))))
#|

def fib(number):
    if number <= 1:
        return number
    return(fib(number-1) + fib(number-2))
# def test_invoke_strict_module_recursive(self):
#     codestr = """
#         def fib(number):
#             if number <= 1:
#                 return number
#             return(fib(number-1) + fib(number-2))
#     """
#     with self.in_strict_module(codestr) as mod:
#         fib = mod.fib
#         self.assertInBytecode(
#             fib,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "fib"), 1),
#         )
#         self.assertEqual(fib(4), 3)
|#

