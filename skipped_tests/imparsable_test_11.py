# Reason: Format too complicated
def test_richards(self):
    with open(RICHARDS_PATH) as f:
        codestr = f.read()
    with self.in_module(codestr) as mod:
        Richards = mod.Richards
        self.assertTrue(Richards().run(1))
