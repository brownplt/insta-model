def test_bind_instance(self) -> None:
    mod, comp = self.bind_module("class C: pass\na: C = C()")
    assign = mod.body[1]
    types = comp.modules["foo"].types
    self.assertEqual(types[assign.target].name, "foo.C")
    self.assertEqual(repr(types[assign.target]), "<foo.C>")
