# Reason: Test hitted some skipped words
def test_nested_list_comprehensions_with_if(self):
    codestr = """
    from typing import List
    def foo() -> List[int]:
         a = [1, 2, 3, 4]
         b = [1, 2]
         return [x * y for x in a for y in b if x > 2]
    """
    with self.in_module(codestr) as mod:
        f = mod.foo
        self.assertEqual(f(), [3, 6, 4, 8])
