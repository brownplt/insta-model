# Reason: Can't be translated by any of the three translator
def test_checked_dict_items(self):
    x = chkdict[str, int](x=2)
    self.assertEqual(
        list(x.items()),
        [
            ("x", 2),
        ],
    )
    x = chkdict[str, int](x=2, y=3)
    self.assertEqual(list(x.items()), [("x", 2), ("y", 3)])
