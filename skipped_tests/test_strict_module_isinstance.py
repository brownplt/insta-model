def test_strict_module_isinstance(self):
    code = """
        from typing import Optional
        def foo(tval: Optional[object]) -> str:
            if isinstance(tval, str):
                return tval
            return "hi"
    """
    self.compile_strict(code)
