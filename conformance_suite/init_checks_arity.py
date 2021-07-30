# init_checks_arity.py
# This should fail.

class Person:
    def __init__(self, name: str, age: int):
        pass

p1 = Person('Alice', 21, False)