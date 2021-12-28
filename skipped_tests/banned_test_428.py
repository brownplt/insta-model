# Reason: Test hitted a banned word Array
def test_vector_invalid_type(self):
    class C:
        pass
    with self.assertRaisesRegex(
        TypeError, "Invalid type for ArrayElement: C when instantiating Vector"
    ):
        Vector[C]
