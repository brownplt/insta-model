# Reason: Code hitted some skipped words
def test_list_comprehension_with_if(self):
    codestr = """
    from typing import List
    def foo() -> List[int]:
         a = [1, 2, 3, 4]
         return [x for x in a if x > 2]
    """
    with self.in_module(codestr) as mod:
        f = mod.foo
        self.assertEqual(f(), [3, 4])
