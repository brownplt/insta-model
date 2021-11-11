# Reason: Format too complicated
def test_type_is_exact(self) -> None:
    self.assertTrue(FUNCTION_TYPE.is_exact)
    self.assertTrue(METHOD_TYPE.is_exact)
    self.assertTrue(MEMBER_TYPE.is_exact)
    self.assertTrue(BUILTIN_METHOD_DESC_TYPE.is_exact)
    self.assertTrue(BUILTIN_METHOD_TYPE.is_exact)
    self.assertTrue(SLICE_TYPE.is_exact)
    self.assertTrue(NONE_TYPE.is_exact)
    self.assertTrue(STR_EXACT_TYPE.is_exact)
    self.assertTrue(INT_EXACT_TYPE.is_exact)
    self.assertTrue(FLOAT_EXACT_TYPE.is_exact)
    self.assertTrue(COMPLEX_EXACT_TYPE.is_exact)
    self.assertTrue(BOOL_TYPE.is_exact)
    self.assertTrue(ELLIPSIS_TYPE.is_exact)
    self.assertTrue(DICT_EXACT_TYPE.is_exact)
    self.assertTrue(TUPLE_EXACT_TYPE.is_exact)
    self.assertTrue(SET_EXACT_TYPE.is_exact)
    self.assertTrue(LIST_EXACT_TYPE.is_exact)
    self.assertFalse(TYPE_TYPE.is_exact)
    self.assertFalse(OBJECT_TYPE.is_exact)
    self.assertFalse(DYNAMIC_TYPE.is_exact)
    self.assertFalse(STR_TYPE.is_exact)
    self.assertFalse(INT_TYPE.is_exact)
    self.assertFalse(FLOAT_TYPE.is_exact)
    self.assertFalse(COMPLEX_TYPE.is_exact)
    self.assertFalse(BYTES_TYPE.is_exact)
    self.assertFalse(DICT_TYPE.is_exact)
    self.assertFalse(TUPLE_TYPE.is_exact)
    self.assertFalse(SET_TYPE.is_exact)
    self.assertFalse(LIST_TYPE.is_exact)
    self.assertFalse(BASE_EXCEPTION_TYPE.is_exact)
    self.assertFalse(EXCEPTION_TYPE.is_exact)
    self.assertFalse(STATIC_METHOD_TYPE.is_exact)
    self.assertFalse(NAMED_TUPLE_TYPE.is_exact)
