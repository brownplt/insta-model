# Reason: Test hitted a banned word int64
def test_vector_generics(self):
    T = TypeVar("T")
    VT = Vector[T]
    VT2 = VT[int64]
    a = VT2()
    a.append(42)
    with self.assertRaisesRegex(TypeError, "Cannot create plain Vector"):
        VT()
