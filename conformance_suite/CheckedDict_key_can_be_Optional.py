# CheckedDict_key_can_be_Optional.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
from typing import Optional

x: CheckedDict[Optional[str], int] = CheckedDict[Optional[str], int]({
    "foo": 2,
    None: 3
})
assert x[None] is 3