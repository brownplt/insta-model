# Reason: Hitted a skipped word (xxclassloader)
def test_generic_type_inst_optional_okay_func(self):
    from xxclassloader import spamobj
    o = spamobj[Optional[str]]()
    f = o.setstate
    f("abc")
    f(None)
