# Reason: Test hitted some skipped words
def test_type_type_final(self):
    codestr = """
    class A(type):
        pass
    """
    self.compile(codestr)
