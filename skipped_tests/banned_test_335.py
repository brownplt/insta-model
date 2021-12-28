# Reason: Test hitted a banned word xxclassloader
def test_generic_type_def_non_type(self):
    from xxclassloader import spamobj
    with self.assertRaises(TypeError):
        spamobj[42]
