# Reason: Test hitted a banned word int64
def test_inlined_nodes_have_line_info(self):
    self.type_error(
        """
        from __static__ import int64, cbool, inline
        @inline
        def x(i: int64) -> cbool:
            return i == "foo"
        def foo(i: int64) -> cbool:
            return x(i)
        """,
        "",
        at="i ==",
    )
