# Reason: Can't be translated by any of the three translator
def test_checked_dict___module__(self):
    class Lol:
        pass
    x = chkdict[int, Lol]()
    self.assertEqual(type(x).__module__, "__static__")
