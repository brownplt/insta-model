def test_final_callable_protocol_retains_inferred_type(self):
    codestr = """
    from typing import Final, Protocol
    def foo(x: int) -> str:
        return "A"
    class CallableProtocol(Protocol):
        def __call__(self, x: int) -> str:
            pass
    f: Final[CallableProtocol] = foo
    def bar(x: int) -> str:
        return f(x)
    """
    with self.in_module(codestr) as mod:
        f = mod.bar
        self.assertInBytecode(f, "INVOKE_FUNCTION")
