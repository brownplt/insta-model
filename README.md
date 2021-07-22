# insta-model

## The goal

The long-term goal is to model an idealized StaticPython with PLT Redex.

The short-term goal is to model a subset of StaticPython that includes the following types

* the dynamic type
* base types
  * None
  * bool
  * int
  * str
* `CheckedDict[T0, T1]`
* (user-defined) classes

and the following optimizations

* Typed slots
* Vtables

## How to achieve the goal?

I will start by creating a test suite. Ideally, the test suite will include some tests from [StaticPython's comformance suite](./test_static.py) and some hand-craft tests to cover corner cases when necessary.