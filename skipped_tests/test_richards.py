# Reason: Can't be translated by any of the three translator
def test_richards(self):
    with open(RICHARDS_PATH) as f:
        codestr = f.read()
    with self.in_module(codestr) as mod:
        Richards = mod.Richards
        self.assertTrue(Richards().run(1))
