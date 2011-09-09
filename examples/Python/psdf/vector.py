import math
import yaml

class Vector(list, yaml.YAMLObject):
    """Basic vector type."""

    def __repr__(self):
        return 'Vector(%r)'%list(self)

    def __add__(self, other):
        if isinstance(other, list):
            return Vector([x+y for x,y in zip(self, other)])
        else:
            return Vector([x + other for x in self])

    def __radd__(self, other):
        return self + other

    def __mul__(self, other):
        if isinstance(other, list):
            return sum([x*y for x,y in zip(self,other)])
        else:
            return Vector([x*other for x in self])

    def __rmul__(self, other):
        return self * other

    def __sub__(self, other):
        if isinstance(other, list):
            return Vector([x-y for x,y in zip(self, other)])
        else:
            return Vector([x-other for x in self])

    def __rsub__(self, other):
        if isinstance(other, list):
            return Vector([y-x for x,y in zip(self,other)])
        else:
            return Vector([other-x for x in self])

    def __div__(self, other):
        return Vector([x/other for x in self])

    def __iadd__(self, other):
        if isinstance(other, list):
            for i in range(len(other)):
                self[i] += other[i]
        else:
            for i in range(len(self)):
                self[i] += other

        return self

    def __isub__(self, other):
        if isinstance(other, list):
            for i in range(len(other)):
                self[i] -= other[i]
        else:
            for i in range(len(self)):
                self[i] -= other

        return self

    def __imul__(self, other):
        for i in range(len(self)):
            self[i] *= other

        return self

    def __idiv__(self, other):
        for i in range(len(self)):
            self[i] /= other

        return self

    def norm(self):
        return math.sqrt(self*self)
