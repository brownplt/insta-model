def test_final_annotation_nesting(self):
    with self.assertRaisesRegex(
        TypedSyntaxError, "Final annotation is only valid in initial declaration"
    ):
        self.compile(
            """
            from typing import Final, List
            x: List[Final[str]] = []
            """,
            modname="foo",
        )
    with self.assertRaisesRegex(
        TypedSyntaxError, "Final annotation is only valid in initial declaration"
    ):
        self.compile(
            """
            from typing import Final, List
            x: List[int | Final] = []
            """,
            modname="foo",
        )
