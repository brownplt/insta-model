# Reason: Hitted a skipped word (xxclassloader)
def test_generic_type_inst_okay_func(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    f = o.setstate
    f("abc")
