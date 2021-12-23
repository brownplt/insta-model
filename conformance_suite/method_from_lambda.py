# method_from_lambda.py
# This should pass.
# This should terminate.

from typing import Any, ClassVar


class C:
    m: ClassVar[Any] = lambda self: 2


obj = C()
assert obj.m() is 2