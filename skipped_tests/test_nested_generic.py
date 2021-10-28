def test_nested_generic(self):
    S = TypeVar("S")
    T = TypeVar("T")
    U = TypeVar("U")
    class F(StaticGeneric[U]):
        pass
    class C(StaticGeneric[T]):
        pass
    A = F[S]
    self.assertEqual(A.__parameters__, (S,))
    X = C[F[T]]
    self.assertEqual(X.__parameters__, (T,))
