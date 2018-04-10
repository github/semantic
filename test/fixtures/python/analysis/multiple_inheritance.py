class Foo:
    def dang(self):
        return "foo!"

class Bar:
    def dang(self):
        return "bar!"


class Baz(Foo, Bar): pass

Baz.dang()
