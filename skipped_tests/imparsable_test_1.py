# Reason: Format too complicated
def test_type_attrs(self):
    attrs = TYPE_TYPE.__dict__.keys()
    obj_attrs = OBJECT_TYPE.__dict__.keys()
    self.assertEqual(set(attrs), set(obj_attrs))
