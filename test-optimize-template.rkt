#lang racket
(require redex)
(require redex-abbrevs)
(require "desugar.rkt")
(require "compile.rkt")

(define-language Matcher
  (Any hole
  (any ... Any any ...)))

;; conformance_suite/test_augassign_inexact.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def something () dynamic (begin (return 3))) (def t () dynamic (begin (define/assign a int (something)) (define/assign b dynamic 0) (define/assign b dynamic (bin-op + b a)) (return b))))))))
#|

def something():
    return 3
def t():
    a: int = something()
    b = 0
    b += a
    return b
# def test_augassign_inexact(self):
#     codestr = """
#     def something():
#         return 3
#     def t():
#         a: int = something()
#         b = 0
#         b += a
#         return b
#     """
#     with self.in_module(codestr) as mod:
#         t = mod.t
#         self.assertInBytecode(t, "INPLACE_ADD")
#         self.assertEqual(t(), 3)
|#


;; conformance_suite/test_compile_checked_dict_len.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (1 "abc")))) (return (len x)))))))))
#|

from __static__ import CheckedDict
def testfunc():
    x = CheckedDict[int, str]({1:'abc'})
    return len(x)
# def test_compile_checked_dict_len(self):
#     codestr = """
#         from __static__ import CheckedDict
#         def testfunc():
#             x = CheckedDict[int, str]({1:'abc'})
#             return len(x)
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         self.assertInBytecode(test, "FAST_LEN", FAST_LEN_DICT)
#         if cinderjit is not None:
#             cinderjit.get_and_clear_runtime_stats()
#         self.assertEqual(test(), 1)
#         if cinderjit is not None:
#             stats = cinderjit.get_and_clear_runtime_stats().get("deopt")
#             self.assertFalse(stats)
|#


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


;; conformance_suite/test_compile_dict_setitem_subscr.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (1 "abc")))) (define/assign (subscript x 2) dynamic "def") (return x))))))))
#|

from __static__ import CheckedDict
def testfunc():
    x = CheckedDict[int, str]({1:'abc'})
    x[2] = 'def'
    return x
# def test_compile_dict_setitem_subscr(self):
#     codestr = """
#         from __static__ import CheckedDict
#         def testfunc():
#             x = CheckedDict[int, str]({1:'abc'})
#             x[2] = 'def'
#             return x
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         x = test()
#         self.assertInBytecode(
#             test,
#             "INVOKE_METHOD",
#             (
#                 (
#                     "__static__",
#                     "chkdict",
#                     (("builtins", "int"), ("builtins", "str")),
#                     "__setitem__",
#                 ),
#                 2,
#             ),
#         )
#         self.assertEqual(x, {1: "abc", 2: "def"})
|#


;; conformance_suite/test_compile_nested_class_in_fn.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def fn () dynamic (begin (class C (object) (field "c" int 1)) (return (C)))))))))
#|

def fn():
    class C:
        c: int = 1
    return C()
# def test_compile_nested_class_in_fn(self):
#     codestr = """
#     def fn():
#         class C:
#             c: int = 1
#         return C()
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.fn
#         self.assertInBytecode(f, "CALL_FUNCTION")
|#


;; conformance_suite/test_dict_invoke.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "__static__" ("pydict")) (def f ((x dynamic)) dynamic (begin (define/assign y pydict x) (return ((attribute y "get") "foo")))))))))
#|

from __static__ import pydict
def f(x):
    y: pydict = x
    return y.get('foo')
# def test_dict_invoke(self):
#     codestr = """
#         from __static__ import pydict
#         def f(x):
#             y: pydict = x
#             return y.get('foo')
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "INVOKE_METHOD", (("builtins", "dict", "get"), 1))
#         self.assertEqual(f({}), None)
|#


;; conformance_suite/test_dict_invoke_ret.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "__static__" ("pydict")) (def g () dynamic (begin (return None))) (def f ((x dynamic)) dynamic (begin (define/assign y pydict x) (define/assign z dynamic ((attribute y "get") "foo")) (define/assign z dynamic None) (return z))))))))
#|

from __static__ import pydict
def g(): return None
def f(x):
    y: pydict = x
    z = y.get('foo')
    z = None  # should be typed to dynamic
    return z
# def test_dict_invoke_ret(self):
#     codestr = """
#         from __static__ import pydict
#         def g(): return None
#         def f(x):
#             y: pydict = x
#             z = y.get('foo')
#             z = None  # should be typed to dynamic
#             return z
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "INVOKE_METHOD", (("builtins", "dict", "get"), 1))
#         self.assertEqual(f({}), None)
|#


;; conformance_suite/test_final_constant_folding_disabled_on_nonfinals.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "typing" ("Final")) (define/assign X str "omg") (def f () str (begin (return (subscript X 1)))))))))
#|

from typing import Final
X: str = "omg"
def f() -> str:
    return X[1]
# def test_final_constant_folding_disabled_on_nonfinals(self):
#     codestr = """
#     from typing import Final
#     X: str = "omg"
#     def f() -> str:
#         return X[1]
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "LOAD_CONST", "omg")
#         self.assertInBytecode(f, "LOAD_GLOBAL", "X")
#         self.assertEqual(f(), "m")
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


;; conformance_suite/test_inline_nested.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "__static__" ("inline")) (def e ((x dynamic) (y dynamic)) dynamic (begin (return (bin-op + x y)))) (def f ((x dynamic) (y dynamic)) dynamic (begin (return (e x 3)))) (def g () dynamic (begin (return (f 1 2)))))))))
#|

from __static__ import inline
@inline
def e(x, y):
    return x + y
@inline
def f(x, y):
    return e(x, 3)
def g():
    return f(1,2)
# def test_inline_nested(self):
#     codestr = """
#         from __static__ import inline
#         @inline
#         def e(x, y):
#             return x + y
#         @inline
#         def f(x, y):
#             return e(x, 3)
#         def g():
#             return f(1,2)
#     """
#     with self.in_module(codestr, optimize=2) as mod:
#         g = mod.g
#         self.assertInBytecode(g, "LOAD_CONST", 4)
#         self.assertEqual(g(), 4)
|#


;; conformance_suite/test_inline_nested_arg.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "__static__" ("inline")) (def e ((x dynamic) (y dynamic)) dynamic (begin (return (bin-op + x y)))) (def f ((x dynamic) (y dynamic)) dynamic (begin (return (e x 3)))) (def g ((a dynamic) (b dynamic)) dynamic (begin (return (f a b)))))))))
#|

from __static__ import inline
@inline
def e(x, y):
    return x + y
@inline
def f(x, y):
    return e(x, 3)
def g(a,b):
    return f(a,b)
# def test_inline_nested_arg(self):
#     codestr = """
#         from __static__ import inline
#         @inline
#         def e(x, y):
#             return x + y
#         @inline
#         def f(x, y):
#             return e(x, 3)
#         def g(a,b):
#             return f(a,b)
#     """
#     with self.in_module(codestr, optimize=2) as mod:
#         g = mod.g
#         self.assertInBytecode(g, "LOAD_CONST", 3)
#         self.assertInBytecode(g, "BINARY_ADD")
#         self.assertEqual(g(1, 2), 4)
|#


;; conformance_suite/test_inline_recursive.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "__static__" ("inline")) (def f ((x dynamic) (y dynamic)) dynamic (begin (return (f x y)))) (def g () dynamic (begin (return (f 1 2)))))))))
#|

from __static__ import inline
@inline
def f(x, y):
    return f(x, y)
def g():
    return f(1,2)
# def test_inline_recursive(self):
#     codestr = """
#         from __static__ import inline
#         @inline
#         def f(x, y):
#             return f(x, y)
#         def g():
#             return f(1,2)
#     """
#     with self.in_module(codestr, optimize=2) as mod:
#         g = mod.g
#         self.assertInBytecode(g, "INVOKE_FUNCTION", (((mod.__name__, "f"), 2)))
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


;; conformance_suite/test_max.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def f ((a int) (b int)) int (begin (return (max a b)))))))))
#|

def f(a: int, b: int) -> int:
    return max(a, b)
# def test_max(self):
#     codestr = """
#         def f(a: int, b: int) -> int:
#             return max(a, b)
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "COMPARE_OP", ">=")
#         self.assertInBytecode(f, "POP_JUMP_IF_FALSE")
#         self.assertEqual(f(1, 3), 3)
#         self.assertEqual(f(3, 1), 3)
|#


;; conformance_suite/test_max_stability.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def f ((a int) (b int)) int (begin (return (max a b)))))))))
#|

def f(a: int, b: int) -> int:
    return max(a, b)
# def test_max_stability(self):
#     codestr = """
#         def f(a: int, b: int) -> int:
#             return max(a, b)
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "COMPARE_OP", ">=")
#         self.assertInBytecode(f, "POP_JUMP_IF_FALSE")
#         # p & q should be different objects, but with same value
#         p = int("11334455667")
#         q = int("11334455667")
#         self.assertNotEqual(id(p), id(q))
#         # Since p and q are equal, the returned value should be the first arg
#         self.assertEqual(id(f(p, q)), id(p))
#         self.assertEqual(id(f(q, p)), id(q))
|#


;; conformance_suite/test_method_prologue_no_annotation.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def f ((x dynamic)) dynamic (begin (return 42))))))))
#|

def f(x):
    return 42
# def test_method_prologue_no_annotation(self):
#     codestr = """
#     def f(x):
#         return 42
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "CHECK_ARGS", ())
#         self.assertEqual(f("abc"), 42)
|#


;; conformance_suite/test_min.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def f ((a int) (b int)) int (begin (return (min a b)))))))))
#|

def f(a: int, b: int) -> int:
    return min(a, b)
# def test_min(self):
#     codestr = """
#         def f(a: int, b: int) -> int:
#             return min(a, b)
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "COMPARE_OP", "<=")
#         self.assertInBytecode(f, "POP_JUMP_IF_FALSE")
#         self.assertEqual(f(1, 3), 1)
#         self.assertEqual(f(3, 1), 1)
|#


;; conformance_suite/test_min_stability.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def f ((a int) (b int)) int (begin (return (min a b)))))))))
#|

def f(a: int, b: int) -> int:
    return min(a, b)
# def test_min_stability(self):
#     codestr = """
#         def f(a: int, b: int) -> int:
#             return min(a, b)
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "COMPARE_OP", "<=")
#         self.assertInBytecode(f, "POP_JUMP_IF_FALSE")
#         # p & q should be different objects, but with same value
#         p = int("11334455667")
#         q = int("11334455667")
#         self.assertNotEqual(id(p), id(q))
#         # Since p and q are equal, the returned value should be the first arg
#         self.assertEqual(id(f(p, q)), id(p))
#         self.assertEqual(id(f(q, p)), id(q))
|#


;; conformance_suite/test_no_narrow_to_dynamic.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def f () dynamic (begin (return 42))) (def g () dynamic (begin (define/assign x int 100) (define/assign x dynamic (f)) (return ((attribute x "bit_length"))))))))))
#|

def f():
    return 42
def g():
    x: int = 100
    x = f()
    return x.bit_length()
# def test_no_narrow_to_dynamic(self):
#     codestr = """
#         def f():
#             return 42
#         def g():
#             x: int = 100
#             x = f()
#             return x.bit_length()
#     """
#     with self.in_module(codestr) as mod:
#         g = mod.g
#         self.assertInBytecode(g, "CAST", ("builtins", "int"))
#         self.assertInBytecode(
#             g, "INVOKE_METHOD", (("builtins", "int", "bit_length"), 0)
#         )
#         self.assertEqual(g(), 6)
|#


;; conformance_suite/test_none_not.py
(test-match SP-compiled any (term (compile-program (desugar-program ((def t () bool (begin (define/assign x dynamic None) (if (unary-op not x) (begin (return #t)) (begin (return #f))))))))))
#|

def t() -> bool:
    x = None
    if not x:
        return True
    else:
        return False
# def test_none_not(self):
#     codestr = """
#     def t() -> bool:
#         x = None
#         if not x:
#             return True
#         else:
#             return False
#     """
#     with self.in_module(codestr) as mod:
#         t = mod.t
#         self.assertInBytecode(t, "POP_JUMP_IF_TRUE")
#         self.assertTrue(t())
|#


;; conformance_suite/test_package_no_parent.py
(test-match SP-compiled any (term (compile-program (desugar-program ((class C (object) (method "f" self () dynamic (begin (return 42)))))))))
#|

class C:
    def f(self):
        return 42
# def test_package_no_parent(self):
#     codestr = """
#         class C:
#             def f(self):
#                 return 42
#     """
#     with self.in_module(codestr, name="package_no_parent.child") as mod:
#         C = mod.C
#         self.assertInBytecode(
#             C.f, "CHECK_ARGS", (0, ("package_no_parent.child", "C"))
#         )
#         self.assertEqual(C().f(), 42)
|#


;; conformance_suite/test_ret_type_cast.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "typing" ("Any")) (def testfunc ((x str) (y str)) bool (begin (return (== x y)))))))))
#|

from typing import Any
def testfunc(x: str, y: str) -> bool:
    return x == y
# def test_ret_type_cast(self):
#     codestr = """
#         from typing import Any
#         def testfunc(x: str, y: str) -> bool:
#             return x == y
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.testfunc
#         self.assertEqual(f("abc", "abc"), True)
#         self.assertInBytecode(f, "CAST", ("builtins", "bool"))
|#


;; conformance_suite/test_unknown_isinstance_narrows.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "typing" ("Any")) (class C (object) (method "__init__" self ((x str)) dynamic (begin (define/assign (attribute self "x") str x)))) (def testfunc ((x dynamic)) dynamic (begin (if (isinstance x C) (begin (return (attribute x "x"))) (begin)))))))))
#|

from typing import Any
class C:
    def __init__(self, x: str):
        self.x: str = x
def testfunc(x):
    if isinstance(x, C):
        return x.x
# def test_unknown_isinstance_narrows(self):
#     codestr = """
#         from typing import Any
#         class C:
#             def __init__(self, x: str):
#                 self.x: str = x
#         def testfunc(x):
#             if isinstance(x, C):
#                 return x.x
#     """
#     with self.in_module(codestr) as mod:
#         testfunc = mod.testfunc
#         self.assertInBytecode(testfunc, "LOAD_FIELD", (mod.__name__, "C", "x"))
|#


;; conformance_suite/test_unknown_isinstance_narrows_class_attr.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "typing" ("Any")) (class C (object) (method "__init__" self ((x str)) dynamic (begin (define/assign (attribute self "x") str x))) (method "f" self ((other dynamic)) str (begin (if (isinstance other (attribute self "__class__")) (begin (return (attribute other "x"))) (begin)) (return "")))))))))
#|

from typing import Any
class C:
    def __init__(self, x: str):
        self.x: str = x
    def f(self, other) -> str:
        if isinstance(other, self.__class__):
            return other.x
        return ''
# def test_unknown_isinstance_narrows_class_attr(self):
#     codestr = """
#         from typing import Any
#         class C:
#             def __init__(self, x: str):
#                 self.x: str = x
#             def f(self, other) -> str:
#                 if isinstance(other, self.__class__):
#                     return other.x
#                 return ''
#     """
#     with self.in_module(codestr) as mod:
#         C = mod.C
#         self.assertInBytecode(
#             C.f,
#             "LOAD_FIELD",
#             (mod.__name__, "C", "x"),
#         )
|#


;; conformance_suite/test_unknown_isinstance_narrows_class_attr_dynamic.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "typing" ("Any")) (class C (object) (method "__init__" self ((x str)) dynamic (begin (define/assign (attribute self "x") str x))) (method "f" self ((other dynamic) (unknown dynamic)) dynamic (begin (if (isinstance other (attribute unknown "__class__")) (begin (return (attribute other "x"))) (begin)) (return "")))))))))
#|

from typing import Any
class C:
    def __init__(self, x: str):
        self.x: str = x
    def f(self, other, unknown):
        if isinstance(other, unknown.__class__):
            return other.x
        return ''
# def test_unknown_isinstance_narrows_class_attr_dynamic(self):
#     codestr = """
#         from typing import Any
#         class C:
#             def __init__(self, x: str):
#                 self.x: str = x
#             def f(self, other, unknown):
#                 if isinstance(other, unknown.__class__):
#                     return other.x
#                 return ''
#     """
#     with self.in_module(codestr) as mod:
#         C = mod.C
#         self.assertInBytecode(C.f, "LOAD_ATTR", "x")
|#


;; conformance_suite/test_unknown_param_ann.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "typing" ("Any")) (class C (object) (method "__init__" self ((x str)) dynamic (begin (define/assign (attribute self "x") str x))) (method "__eq__" self ((other Any)) bool (begin (return #f)))))))))
#|

from typing import Any
class C:
    def __init__(self, x: str):
        self.x: str = x
    def __eq__(self, other: Any) -> bool:
        return False
# def test_unknown_param_ann(self):
#     codestr = """
#         from typing import Any
#         class C:
#             def __init__(self, x: str):
#                 self.x: str = x
#             def __eq__(self, other: Any) -> bool:
#                 return False
#     """
#     with self.in_module(codestr) as mod:
#         C = mod.C
#         x = C("abc")
#         self.assertInBytecode(C.__eq__, "CHECK_ARGS", (0, (mod.__name__, "C")))
#         self.assertNotEqual(x, x)
|#

