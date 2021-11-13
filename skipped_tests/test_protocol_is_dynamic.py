# Reason: Test hitted some skipped words
def test_protocol_is_dynamic(self):
    codestr = """
    from typing import Protocol
    class CallableProtocol(Protocol):
        def __call__(self, x: int) -> str:
            pass
    def foo(x: str) -> int:
        return int(x)
    c: CallableProtocol = foo
    """
    with self.in_module(codestr) as mod:
        c = mod.c
        self.assertEqual(c("1"), 1)
