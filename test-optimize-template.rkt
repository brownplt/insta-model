#lang racket
(require redex/reduction-semantics)
(require redex-abbrevs)
(require "desugar.rkt")
(require "compile.rkt")

(define-language Matcher
  (Any hole
  (any ... Any any ...)))

;; conformance_suite/test_assign_constant_to_object.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "f" () dynamic ((ann-assign "x" "object" (bin-op + (con 42) (con 1))))))))))
#|
def f():
    x: object = 42 + 1
# def test_assign_constant_to_object(self):
#     codestr = """
#         def f():
#             x: object = 42 + 1
#     """
#     code = self.compile(codestr, modname="foo")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "CAST", ("builtins", "object"))
|#


;; conformance_suite/test_assign_dynamic_to_object.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "f" (("C" dynamic)) dynamic ((ann-assign "x" "object" (call "C" ())))))))))
#|
def f(C):
    x: object = C()
# def test_assign_dynamic_to_object(self):
#     codestr = """
#         def f(C):
#             x: object = C()
#     """
#     code = self.compile(codestr, modname="foo")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "CAST", ("builtins", "object"))
|#


;; conformance_suite/test_assign_num_to_object.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "f" () dynamic ((ann-assign "x" "object" (con 42)))))))))
#|
def f():
    x: object = 42
# def test_assign_num_to_object(self):
#     codestr = """
#         def f():
#             x: object = 42
#     """
#     code = self.compile(codestr, modname="foo")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "CAST", ("builtins", "object"))
|#


;; conformance_suite/test_compare_subclass.py
(test-match SP-compiled any (term (compile-program (desugar-program ((class "C" () (pass)) (class "D" ("C") (pass)) (assign ("x") (compare (call "C" ()) ((> (call "D" ()))))))))))
#|
class C: pass
class D(C): pass
x = C() > D()
# def test_compare_subclass(self):
#     codestr = """
#     class C: pass
#     class D(C): pass
#     x = C() > D()
#     """
#     code = self.compile(codestr)
#     self.assertInBytecode(code, "COMPARE_OP")
|#


;; conformance_suite/test_exact_invoke_function.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "f" () "str" ((return (call (attribute (con ", ") "join") ((list ((con "1") (con "2") (con "3")))))))))))))
#|
def f() -> str:
    return ", ".join(['1','2','3'])
# def test_exact_invoke_function(self):
#     codestr = """
#         def f() -> str:
#             return ", ".join(['1','2','3'])
#     """
#     f = self.find_code(self.compile(codestr))
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(
#             f, "INVOKE_FUNCTION", (("builtins", "str", "join"), 2)
#         )
#         f()
|#


;; conformance_suite/test_multiply_list_exact_by_int.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "f" () "int" ((assign ("l") (bin-op * (list ((con 1) (con 2) (con 3))) (con 2))) (return (call "len" ("l"))))))))))
#|
def f() -> int:
    l = [1, 2, 3] * 2
    return len(l)
# def test_multiply_list_exact_by_int(self):
#     codestr = """
#         def f() -> int:
#             l = [1, 2, 3] * 2
#             return len(l)
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "FAST_LEN", FAST_LEN_LIST)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f(), 6)
|#


;; conformance_suite/test_multiply_list_exact_by_int_reverse.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "f" () "int" ((assign ("l") (bin-op * (con 2) (list ((con 1) (con 2) (con 3))))) (return (call "len" ("l"))))))))))
#|
def f() -> int:
    l = 2 * [1, 2, 3]
    return len(l)
# def test_multiply_list_exact_by_int_reverse(self):
#     codestr = """
#         def f() -> int:
#             l = 2 * [1, 2, 3]
#             return len(l)
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "FAST_LEN", FAST_LEN_LIST)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f(), 6)
|#


;; conformance_suite/test_nonarray_len.py
(test-match SP-compiled any (term (compile-program (desugar-program ((class "Lol" () ((function-def "__len__" (("self" dynamic)) dynamic ((return (con 421)))))) (function-def "y" () dynamic ((return (call "len" ((call "Lol" ())))))))))))
#|
class Lol:
    def __len__(self):
        return 421
def y():
    return len(Lol())
# def test_nonarray_len(self):
#     codestr = """
#         class Lol:
#             def __len__(self):
#                 return 421
#         def y():
#             return len(Lol())
#     """
#     y = self.find_code(self.compile(codestr, modname="foo"), name="y")
#     self.assertNotInBytecode(y, "FAST_LEN")
#     with self.in_module(codestr) as mod:
#         y = mod.y
#         self.assertEqual(y(), 421)
|#


;; conformance_suite/test_seq_repeat_inexact_list.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "typing" ("List")) (function-def "f" (("l" (subscript "List" "int"))) dynamic ((return (bin-op * "l" (con 2))))))))))
#|
from typing import List
def f(l: List[int]):
    return l * 2
# def test_seq_repeat_inexact_list(self):
#     codestr = """
#         from typing import List
#         def f(l: List[int]):
#             return l * 2
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "SEQUENCE_REPEAT", SEQ_LIST | SEQ_REPEAT_INEXACT_SEQ)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f([1, 2]), [1, 2, 1, 2])
#         class MyList(list):
#             def __mul__(self, other):
#                 return "RESULT"
#         self.assertEqual(mod.f(MyList([1, 2])), "RESULT")
|#


;; conformance_suite/test_seq_repeat_inexact_num.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "f" (("num" "int")) dynamic ((return (bin-op * "num" (list ((con 1) (con 2))))))))))))
#|
def f(num: int):
    return num * [1, 2]
# def test_seq_repeat_inexact_num(self):
#     codestr = """
#         def f(num: int):
#             return num * [1, 2]
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(
#         f,
#         "SEQUENCE_REPEAT",
#         SEQ_LIST | SEQ_REPEAT_INEXACT_NUM | SEQ_REPEAT_REVERSED,
#     )
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f(2), [1, 2, 1, 2])
#         class MyInt(int):
#             def __mul__(self, other):
#                 return "RESULT"
#         self.assertEqual(mod.f(MyInt(2)), "RESULT")
|#


;; conformance_suite/test_seq_repeat_inexact_tuple.py
(test-match SP-compiled any (term (compile-program (desugar-program ((import-from "typing" ("Tuple")) (function-def "f" (("t" (subscript "Tuple" "int"))) dynamic ((return (bin-op * "t" (con 2))))))))))
#|
from typing import Tuple
def f(t: Tuple[int]):
    return t * 2
# def test_seq_repeat_inexact_tuple(self):
#     codestr = """
#         from typing import Tuple
#         def f(t: Tuple[int]):
#             return t * 2
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "SEQUENCE_REPEAT", SEQ_TUPLE | SEQ_REPEAT_INEXACT_SEQ)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f((1, 2)), (1, 2, 1, 2))
#         class MyTuple(tuple):
#             def __mul__(self, other):
#                 return "RESULT"
#         self.assertEqual(mod.f(MyTuple((1, 2))), "RESULT")
|#


;; conformance_suite/test_seq_repeat_list.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "f" () dynamic ((assign ("l") (list ((con 1) (con 2)))) (return (bin-op * "l" (con 2))))))))))
#|
def f():
    l = [1, 2]
    return l * 2
# def test_seq_repeat_list(self):
#     codestr = """
#         def f():
#             l = [1, 2]
#             return l * 2
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "SEQUENCE_REPEAT", SEQ_LIST)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f(), [1, 2, 1, 2])
|#


;; conformance_suite/test_seq_repeat_list_reversed.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "f" () dynamic ((assign ("l") (list ((con 1) (con 2)))) (return (bin-op * (con 2) "l")))))))))
#|
def f():
    l = [1, 2]
    return 2 * l
# def test_seq_repeat_list_reversed(self):
#     codestr = """
#         def f():
#             l = [1, 2]
#             return 2 * l
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "SEQUENCE_REPEAT", SEQ_LIST | SEQ_REPEAT_REVERSED)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f(), [1, 2, 1, 2])
|#


;; conformance_suite/test_seq_repeat_tuple.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "f" () dynamic ((assign ("t") (tuple ((con 1) (con 2)))) (return (bin-op * "t" (con 2))))))))))
#|
def f():
    t = (1, 2)
    return t * 2
# def test_seq_repeat_tuple(self):
#     codestr = """
#         def f():
#             t = (1, 2)
#             return t * 2
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "SEQUENCE_REPEAT", SEQ_TUPLE)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f(), (1, 2, 1, 2))
|#


;; conformance_suite/test_seq_repeat_tuple_reversed.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "f" () dynamic ((assign ("t") (tuple ((con 1) (con 2)))) (return (bin-op * (con 2) "t")))))))))
#|
def f():
    t = (1, 2)
    return 2 * t
# def test_seq_repeat_tuple_reversed(self):
#     codestr = """
#         def f():
#             t = (1, 2)
#             return 2 * t
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "SEQUENCE_REPEAT", SEQ_TUPLE | SEQ_REPEAT_REVERSED)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f(), (1, 2, 1, 2))
|#


;; conformance_suite/test_typed_swap.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "test" (("a" dynamic)) dynamic ((ann-assign "x" "int") (ann-assign "y" "str") (assign ((tuple ("x" "y"))) (tuple ((con 1) "a"))))))))))
#|
def test(a):
    x: int
    y: str
    x, y = 1, a
# def test_typed_swap(self):
#     codestr = """
#         def test(a):
#             x: int
#             y: str
#             x, y = 1, a
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "CAST", ("builtins", "str"))
#     self.assertNotInBytecode(f, "CAST", ("builtins", "int"))
|#


;; conformance_suite/test_typed_swap_2.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "test" (("a" dynamic)) dynamic ((ann-assign "x" "int") (ann-assign "y" "str") (assign ((tuple ("x" "y"))) (tuple ("a" (con "abc")))))))))))
#|
def test(a):
    x: int
    y: str
    x, y = a, 'abc'
# def test_typed_swap_2(self):
#     codestr = """
#         def test(a):
#             x: int
#             y: str
#             x, y = a, 'abc'
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "CAST", ("builtins", "int"))
#     self.assertNotInBytecode(f, "CAST", ("builtins", "str"))
|#


;; conformance_suite/test_typed_swap_list.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "test" (("a" dynamic)) dynamic ((ann-assign "x" "int") (ann-assign "y" "str") (assign ((list ("x" "y"))) (tuple ("a" (con "abc")))))))))))
#|
def test(a):
    x: int
    y: str
    [x, y] = a, 'abc'
# def test_typed_swap_list(self):
#     codestr = """
#         def test(a):
#             x: int
#             y: str
#             [x, y] = a, 'abc'
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "CAST", ("builtins", "int"))
#     self.assertNotInBytecode(f, "CAST", ("builtins", "str"))
|#


;; conformance_suite/test_typed_swap_member.py
(test-match SP-compiled any (term (compile-program (desugar-program ((class "C" () ((function-def "__init__" (("self" dynamic)) dynamic ((ann-assign (attribute "self" "x") "int" (con 42)))))) (function-def "test" (("a" dynamic)) dynamic ((ann-assign "x" "int") (ann-assign "y" "str") (assign ((tuple ((attribute (call "C" ()) "x") "y"))) (tuple ("a" (con "abc")))))))))))
#|
class C:
    def __init__(self):
        self.x: int = 42
def test(a):
    x: int
    y: str
    C().x, y = a, 'abc'
# def test_typed_swap_member(self):
#     codestr = """
#         class C:
#             def __init__(self):
#                 self.x: int = 42
#         def test(a):
#             x: int
#             y: str
#             C().x, y = a, 'abc'
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"), "test")
#     self.assertInBytecode(f, "CAST", ("builtins", "int"))
#     self.assertNotInBytecode(f, "CAST", ("builtins", "str"))
|#


;; conformance_suite/test_typed_swap_nested.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "test" (("a" dynamic)) dynamic ((ann-assign "x" "int") (ann-assign "y" "str") (ann-assign "z" "str") (assign ((tuple ((tuple ("x" "y")) "z"))) (tuple ((tuple ("a" (con "abc"))) (con "foo")))))))))))
#|
def test(a):
    x: int
    y: str
    z: str
    ((x, y), z) = (a, 'abc'), 'foo'
# def test_typed_swap_nested(self):
#     codestr = """
#         def test(a):
#             x: int
#             y: str
#             z: str
#             ((x, y), z) = (a, 'abc'), 'foo'
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "CAST", ("builtins", "int"))
#     self.assertNotInBytecode(f, "CAST", ("builtins", "str"))
|#


;; conformance_suite/test_typed_swap_nested_2.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "test" (("a" dynamic)) dynamic ((ann-assign "x" "int") (ann-assign "y" "str") (ann-assign "z" "str") (assign ((tuple ((tuple ("x" "y")) "z"))) (tuple ((tuple ((con 1) "a")) (con "foo")))))))))))
#|
def test(a):
    x: int
    y: str
    z: str
    ((x, y), z) = (1, a), 'foo'
# def test_typed_swap_nested_2(self):
#     codestr = """
#         def test(a):
#             x: int
#             y: str
#             z: str
#             ((x, y), z) = (1, a), 'foo'
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "CAST", ("builtins", "str"))
#     self.assertNotInBytecode(f, "CAST", ("builtins", "int"))
|#


;; conformance_suite/test_typed_swap_nested_3.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "test" (("a" dynamic)) dynamic ((ann-assign "x" "int") (ann-assign "y" "int") (ann-assign "z" "str") (assign ((tuple ((tuple ("x" "y")) "z"))) (tuple ((tuple ((con 1) (con 2))) "a"))))))))))
#|
def test(a):
    x: int
    y: int
    z: str
    ((x, y), z) = (1, 2), a
# def test_typed_swap_nested_3(self):
#     codestr = """
#         def test(a):
#             x: int
#             y: int
#             z: str
#             ((x, y), z) = (1, 2), a
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "CAST", ("builtins", "str"))
#     # Currently because the tuple gets turned into a constant this is less than
#     # ideal:
#     self.assertInBytecode(f, "CAST", ("builtins", "int"))
|#


;; conformance_suite/test_unknown_type_binary.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "x" (("a" dynamic) ("b" dynamic)) dynamic ((assign ("z") (bin-op + "a" "b")))))))))
#|
def x(a, b):
    z = a + b
# def test_unknown_type_binary(self):
#     codestr = """
#         def x(a, b):
#             z = a + b
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "BINARY_ADD")
|#


;; conformance_suite/test_unknown_type_compare.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "x" (("a" dynamic) ("b" dynamic)) dynamic ((assign ("z") (compare "a" ((> "b")))))))))))
#|
def x(a, b):
    z = a > b
# def test_unknown_type_compare(self):
#     codestr = """
#         def x(a, b):
#             z = a > b
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "COMPARE_OP")
|#


;; conformance_suite/test_unknown_type_unary.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "x" (("y" dynamic)) dynamic ((assign ("z") (unary-op - "y")))))))))
#|
def x(y):
    z = -y
# def test_unknown_type_unary(self):
#     codestr = """
#         def x(y):
#             z = -y
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "UNARY_NEGATIVE")
|#


;; conformance_suite/test_with_traceback.py
(test-match SP-compiled any (term (compile-program (desugar-program ((function-def "f" () dynamic ((assign ("x") (call "Exception" ())) (return (call (attribute "x" "with_traceback") ((con None)))))))))))
#|
def f():
    x = Exception()
    return x.with_traceback(None)
# def test_with_traceback(self):
#     codestr = """
#         def f():
#             x = Exception()
#             return x.with_traceback(None)
#     """
#     code = self.compile(codestr, modname="foo")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertEqual(type(f()), Exception)
#         self.assertInBytecode(
#             f, "INVOKE_METHOD", (("builtins", "BaseException", "with_traceback"), 1)
#         )
|#

