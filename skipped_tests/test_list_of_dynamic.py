# Reason: Can't be translated by any of the three translator
def test_list_of_dynamic(self):
    codestr = """
        from threading import Thread
        from typing import List
        def f(threads: List[Thread]) -> int:
            return len(threads)
    """
    f = self.find_code(self.compile(codestr), "f")
    self.assertInBytecode(f, "FAST_LEN")
