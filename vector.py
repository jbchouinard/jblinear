"""
This module implements a linear algebra Vector.

>>> Vector([1,2,3])
Vector([1, 2, 3])
"""

import math

import numpy as np


class Vector():
    """
    Create a vector.
    >>> a = Vector([5,5,10])
    >>> a.coordinates
    [5, 5, 10]

    Two vectors are equal if they have the same coordinates.
    >>> b = Vector([5,5,10])
    >>> a == b
    True

    Vectors can be created from iterables
    >>> Vector(range(3)).coordinates
    [0, 1, 2]

    Vectors can be iterated over
    >>> v = Vector([0,5,10])
    >>> [x for x in v]
    [0, 5, 10]
    """
    def __init__(v, coordinates):
        if isinstance(coordinates, np.ndarray):
            v._array = coordinates
        else:
            v._array = np.array(coordinates)
        v._coordinates = list(coordinates)
        v._cache = {}

    def __repr__(v):
        return 'Vector({})'.format(v._coordinates)

    def __eq__(v, w):
        return v._coordinates == w._coordinates

    def __iter__(v):
        return iter(v._coordinates)

    @property
    def coordinates(v):
        return v._coordinates

    @property
    def dimension(v):
        """
        >>> Vector([1,2,3]).dimension
        3
        """
        if 'dimension' not in v._cache:
            v._cache['dimension'] = len(v.coordinates)
        return v._cache['dimension']

    @property
    def magnitude(v):
        """
        >>> Vector([3, 4]).magnitude
        5.0
        """
        if 'magnitude' not in v._cache:
            v._cache['magnitude'] = math.sqrt((v._array ** 2).sum())
        return v._cache['magnitude']

    @property
    def normalized(v):
        """
        >>> Vector([3,0]).normalized
        Vector([1.0, 0.0])
        >>> Vector([3,4.6,12.24]).normalized.magnitude
        1.0
        """
        if 'normalized' not in v._cache:
            if v.magnitude == 0:
                raise ValueError('cannot normalize the zero vector')
            v._cache['normalized'] = v * (1 / v.magnitude)
        return v._cache['normalized']

    def inner(v, w):
        """
        Inner product

        >>> Vector([1,2,3]).inner(Vector([3,2,1]))
        10
        """
        return (v._array * w._array).sum()

    def angle(v, w):
        """
        Angle between vectors

        >>> Vector([1,0]).angle(Vector([0,2]))
        1.5707963267948966
        """
        if (v.magnitude * w.magnitude) == 0:
            raise ValueError('angle undefined for the zero vector')
        return math.acos(v.inner(w) * (1 / (v.magnitude * w.magnitude)))

    def __add__(v, w):
        """
        Vector addition

        >>> Vector([1,1,1]) + Vector([2,2,2])
        Vector([3, 3, 3])
        """
        if not isinstance(w, Vector):
            raise TypeError(
                'unsupported operand type(s) for +: \'%s\' and \'%s\'' %
                (type(v).__name__, type(w).__name__))
        elif v.dimension != w.dimension:
            raise ValueError('cannot add vectors of different dimensions')
        else:
            return Vector(v._array + w._array)

    def __mul__(v, n):
        """
        Vector-scalar multiplication

        >>> Vector([1,2,3]) * 3
        Vector([3, 6, 9])
        """
        return Vector(v._array * n)

    def __rmul__(v, n):
        """
        Scalar-vector multiplication

        >>> 3 * Vector([1,2,3])
        Vector([3, 6, 9])
        """
        return v.__mul__(n)

    def __sub__(v, w):
        """
        Vector substraction

        >>> Vector([10,10,10]) - Vector([3,4,5])
        Vector([7, 6, 5])
        """
        return Vector(v._array - w._array)


if __name__ == '__main__':
    import doctest
    doctest.testmod()
