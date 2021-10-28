def test_type_inexact(self) -> None:
    self.assertIs(LIST_TYPE.inexact(), LIST_TYPE)
    self.assertIs(LIST_EXACT_TYPE.inexact(), LIST_EXACT_TYPE)
    self.assertIs(LIST_TYPE.inexact_type(), LIST_TYPE)
    self.assertIs(LIST_EXACT_TYPE.inexact_type(), LIST_TYPE)
