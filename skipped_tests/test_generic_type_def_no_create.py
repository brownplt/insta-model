# Reason: Hitted a skipped word (xxclassloader)
def test_generic_type_def_no_create(self):
    from xxclassloader import spamobj
    with self.assertRaises(TypeError):
        spamobj()