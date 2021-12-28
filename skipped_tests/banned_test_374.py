# Reason: Test hitted a banned word int64
def test_array_weird_type_construction(self):
    self.assertIs(
        Array[int64],
        Array[
            int64,
        ],
    )
