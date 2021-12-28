# Reason: Test hitted a banned word int64
def test_vector_wrong_arg_count(self):
    class C:
        pass
    with self.assertRaisesRegex(
        TypeError, "Incorrect number of type arguments for Vector"
    ):
        Vector[int64, int64]
