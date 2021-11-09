# CheckedDict_val_can_be_Optional.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
from typing import Optional

x: CheckedDict[str, Optional[int]] = CheckedDict[str, Optional[int]]({
    "foo": 2,
    "bar": None
})
assert x["bar"] is None