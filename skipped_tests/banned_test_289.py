# Reason: Test hitted a banned word int8
def test_primitive_args_many_args(self):
    codestr = """
        from __static__ import int8, int16, int32, int64, uint8, uint16, uint32, uint64, box
        def x(i8: int8, i16: int16, i32: int32, i64: int64, u8: uint8, u16: uint16, u32: uint32, u64: uint64):
            return box(i8), box(i16), box(i32), box(i64), box(u8), box(u16), box(u32), box(u64)
        def y():
            return x(1,2,3,4,5,6,7,8)
    """
    with self.in_strict_module(codestr) as mod:
        self.assertInBytecode(mod.y, "INVOKE_FUNCTION", ((mod.__name__, "x"), 8))
        self.assertEqual(mod.y(), (1, 2, 3, 4, 5, 6, 7, 8))
        self.assertEqual(mod.x(1, 2, 3, 4, 5, 6, 7, 8), (1, 2, 3, 4, 5, 6, 7, 8))
