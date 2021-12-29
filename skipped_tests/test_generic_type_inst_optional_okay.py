# Reason: Hitted a skipped word (xxclassloader)
def test_generic_type_inst_optional_okay(self):
    from xxclassloader import spamobj
    o = spamobj[Optional[str]]()
    o.setstate("abc")
    o.setstate(None)
