# Reason: Test hitted a banned word float
def test_typed_slots_primitives(self):
    slot_types = [
        # signed
        (
            ("__static__", "byte"),
            0,
            1,
            [(1 << 7) - 1, -(1 << 7)],
            [1 << 8],
            ["abc"],
        ),
        (
            ("__static__", "int8"),
            0,
            1,
            [(1 << 7) - 1, -(1 << 7)],
            [1 << 8],
            ["abc"],
        ),
        (
            ("__static__", "int16"),
            0,
            2,
            [(1 << 15) - 1, -(1 << 15)],
            [1 << 15, -(1 << 15) - 1],
            ["abc"],
        ),
        (
            ("__static__", "int32"),
            0,
            4,
            [(1 << 31) - 1, -(1 << 31)],
            [1 << 31, -(1 << 31) - 1],
            ["abc"],
        ),
        (("__static__", "int64"), 0, 8, [(1 << 63) - 1, -(1 << 63)], [], [1 << 63]),
        # unsigned
        (("__static__", "uint8"), 0, 1, [(1 << 8) - 1, 0], [1 << 8, -1], ["abc"]),
        (
            ("__static__", "uint16"),
            0,
            2,
            [(1 << 16) - 1, 0],
            [1 << 16, -1],
            ["abc"],
        ),
        (
            ("__static__", "uint32"),
            0,
            4,
            [(1 << 32) - 1, 0],
            [1 << 32, -1],
            ["abc"],
        ),
        (("__static__", "uint64"), 0, 8, [(1 << 64) - 1, 0], [], [1 << 64]),
        # pointer
        (
            ("__static__", "ssize_t"),
            0,
            self.ptr_size,
            [1, sys.maxsize, -sys.maxsize - 1],
            [],
            [sys.maxsize + 1, -sys.maxsize - 2],
        ),
        # floating point
        (("__static__", "single"), 0.0, 4, [1.0], [], ["abc"]),
        (("__static__", "double"), 0.0, 8, [1.0], [], ["abc"]),
        # misc
        (("__static__", "char"), "\x00", 1, ["a"], [], ["abc"]),
        (("__static__", "cbool"), False, 1, [True], [], ["abc", 1]),
    ]
    target_size = self.base_size + 8
    for type_spec, default, size, test_vals, warn_vals, err_vals in slot_types:
        with self.subTest(
            type_spec=type_spec,
            default=default,
            size=size,
            test_vals=test_vals,
            warn_vals=warn_vals,
            err_vals=err_vals,
        ):
            # Since object sizes are aligned to 8 bytes, figure out how
            # many slots of each type we need to get to 8 bytes.
            self.assertEqual(8 % size, 0)
            num_slots = 8 // size
            class C:
                __slots__ = tuple(f"a{i}" for i in range(num_slots))
                __slot_types__ = {f"a{i}": type_spec for i in range(num_slots)}
            a = C()
            self.assertEqual(sys.getsizeof(a), target_size, type_spec)
            self.assertEqual(a.a0, default)
            self.assertEqual(type(a.a0), type(default))
            for val in test_vals:
                a.a0 = val
                self.assertEqual(a.a0, val)
            with warnings.catch_warnings():
                warnings.simplefilter("error", category=RuntimeWarning)
                for val in warn_vals:
                    with self.assertRaises(RuntimeWarning):
                        a.a0 = val
            for val in err_vals:
                with self.assertRaises((TypeError, OverflowError)):
                    a.a0 = val
