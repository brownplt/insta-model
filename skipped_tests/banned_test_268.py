# Reason: Test hitted a banned word ...
def test_array_types(self):
    codestr = """
        from __static__ import (
            int8,
            int16,
            int32,
            int64,
            uint8,
            uint16,
            uint32,
            uint64,
            char,
            double,
            Array
        )
        from typing import Tuple
        def test() -> Tuple[Array[int64], Array[char], Array[double]]:
            x1: Array[int8] = Array[int8]([1, 3, -5])
            x2: Array[uint8] = Array[uint8]([1, 3, 5])
            x3: Array[int16] = Array[int16]([1, -3, 5])
            x4: Array[uint16] = Array[uint16]([1, 3, 5])
            x5: Array[int32] = Array[int32]([1, 3, 5])
            x6: Array[uint32] = Array[uint32]([1, 3, 5])
            x7: Array[int64] = Array[int64]([1, 3, 5])
            x8: Array[uint64] = Array[uint64]([1, 3, 5])
            x9: Array[char] = Array[char]([ord('a')])
            x10: Array[double] = Array[double]([1.1, 3.3, 5.5])
            # TODO(T92687901): Support Array[single] for array("f", ...).
            arrays = [
                x1,
                x2,
                x3,
                x4,
                x5,
                x6,
                x7,
                x8,
                x9,
                x10,
            ]
            first_elements = []
            for ar in arrays:
                first_elements.append(ar[0])
            return (arrays, first_elements)
    """
    with self.in_module(codestr) as mod:
        test = mod.test
        arrays, first_elements = test()
        exp_arrays = [
            array(*args)
            for args in [
                ("h", [1, 3, -5]),
                ("H", [1, 3, 5]),
                ("i", [1, -3, 5]),
                ("I", [1, 3, 5]),
                ("l", [1, 3, 5]),
                ("L", [1, 3, 5]),
                ("q", [1, 3, 5]),
                ("Q", [1, 3, 5]),
                ("b", [ord("a")]),
                ("d", [1.1, 3.3, 5.5]),
                ("f", [1.1, 3.3, 5.5]),
            ]
        ]
        exp_first_elements = [ar[0] for ar in exp_arrays]
        for result, expectation in zip(arrays, exp_arrays):
            self.assertEqual(result, expectation)
        for result, expectation in zip(first_elements, exp_first_elements):
            self.assertEqual(result, expectation)
