# Reason: Test hitted a banned word ...
def test_assign_to_object(self):
    codestr = """
    def f():
        x: object
        x = None
        x = 1
        x = 'abc'
        x = []
        x = {}
        x = {1, 2}
        x = ()
        x = 1.0
        x = 1j
        x = b'foo'
        x = int
        x = True
        x = NotImplemented
        x = ...
    """
    self.compile(codestr)
