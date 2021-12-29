# Reason: Can't be translated by any of the three translator
def test_generic_type_args(self):
    T = TypeVar("T")
    U = TypeVar("U")
    class C(StaticGeneric[T, U]):
        pass
    c_t = make_generic_type(C, (T, int))
    self.assertEqual(c_t.__parameters__, (T,))
    c_t_s = make_generic_type(c_t, (str,))
    self.assertEqual(c_t_s.__name__, "C[str, int]")
    c_u = make_generic_type(C, (int, U))
    self.assertEqual(c_u.__parameters__, (U,))
    c_u_t = make_generic_type(c_u, (str,))
    self.assertEqual(c_u_t.__name__, "C[int, str]")
    self.assertFalse(hasattr(c_u_t, "__parameters__"))
    c_u_t_1 = make_generic_type(c_u, (int,))
    c_u_t_2 = make_generic_type(c_t, (int,))
    self.assertEqual(c_u_t_1.__name__, "C[int, int]")
    self.assertIs(c_u_t_1, c_u_t_2)
