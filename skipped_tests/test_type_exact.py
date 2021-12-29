# Reason: Can't be translated by any of the three translator
def test_type_exact(self) -> None:
    self.assertIs(LIST_TYPE.exact(), LIST_TYPE)
    self.assertIs(LIST_EXACT_TYPE.exact(), LIST_EXACT_TYPE)
    self.assertIs(LIST_TYPE.exact_type(), LIST_EXACT_TYPE)
    self.assertIs(LIST_EXACT_TYPE.exact_type(), LIST_EXACT_TYPE)
