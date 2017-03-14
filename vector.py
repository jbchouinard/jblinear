"""
This module implements a linear algebra Vector.

>>> Vector([1,2,3])
Vector([1, 2, 3])
"""

import math
from decimal import Decimal

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

    TOLERANCE = 1e-10
    NORM_ZERO_ERR_MSG = 'cannot normalize the zero vector'

    def __init__(v, coordinates, type=None):
        if type:
            coordinates = [type(x) for x in coordinates]
        v._array = np.array(coordinates)
        v._coordinates = list(coordinates)
        v._cache = {}
        v._type = type

    def __repr__(v):
        if v._type is Decimal:
            decs = ["'" + str(dec) + "'" for dec in v._coordinates]
            coord = '[{}]'.format(', '.join(decs))
            return 'Vector({}, type=Decimal)'.format(coord)
        elif v._type:
            return 'Vector({}, type={})'.format(v._coordinates,
                                                v._type.__name__)
        else:
            return 'Vector({})'.format(v._coordinates)

    def __eq__(v, w):
        return (v - w).is_zero()

    def __iter__(v):
        return iter(v._coordinates)

    def __getitem__(v, n):
        if v.dimension <= 3:
            if n == 'x':
                n = 0
            elif n == 'y':
                n = 1
            elif n == 'z':
                n = 2
        return v._coordinates[n]

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
            raise ValueError('addition undefined on vectors of different dimensions')
        else:
            return Vector(v._array + w._array, type=v._mix_types(w))

    def __mul__(v, n):
        """
        Vector-scalar multiplication

        >>> Vector([1,2,3]) * 3
        Vector([3, 6, 9])
        """
        try:
            if v._type is Decimal:
                n = Decimal(n)
            else:
                float(n)
        except ValueError:
            raise TypeError(
                'unsupported operand type(s) for *: \'%s\' and \'%s\'' %
                (type(v).__name__, type(n).__name__))
        return Vector(v._array * n, type=v._type)

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
        if not isinstance(w, Vector):
            raise TypeError(
                'unsupported operand type(s) for -: \'%s\' and \'%s\'' %
                (type(v).__name__, type(w).__name__))
        elif v.dimension != w.dimension:
            raise ValueError('subtraction undefined on vectors of different dimensions')
        else:
            return Vector(v._array - w._array, type=v._mix_types(w))

    def _mix_types(v, w):
        return ((v._type is w._type) and v._type) or None

    @property
    def type(v):
        return v._type

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
            if v._type == Decimal:
                mag = (v._array ** 2).sum().sqrt()
            else:
                mag = math.sqrt((v._array ** 2).sum())
            v._cache['magnitude'] = mag
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
            try:
                v._cache['normalized'] = v * (1 / v.magnitude)
            except ZeroDivisionError:
                raise ValueError(v.NORM_ZERO_ERR_MSG)
        return v._cache['normalized']

    def inner(v, w):
        """
        Inner product v with w

        >>> Vector([1,2,3]).inner(Vector([3,2,1]))
        10
        """
        try:
            return (v._array * w._array).sum()
        except ValueError as e:
            if v.dimension != w.dimension:
                raise ValueError(
                    'inner product undefined on vectors of different dimensions')
            else:
                raise e

    def cross(v, w):
        """
        Cross product

        >>> a = Vector([1.0, 0.0, 0.0])
        >>> b = Vector([0.0, 1.0, 0.0])
        >>> a.cross(b)
        Vector([0.0, 0.0, 1.0])
        >>> b.cross(a)
        Vector([0.0, 0.0, -1.0])
        """
        if not (v.dimension == w.dimension == 3):
            raise ValueError(
                'cross product only defined for 3-dimensional vectors')
        return Vector([
           v['y']*w['z'] - w['y']*v['z'],
           w['x']*v['z'] - v['x']*w['z'],
           v['x']*w['y'] - w['x']*v['y'],
        ])

    def angle(v, w):
        """
        Angle between vectors v and w, in radians

        >>> Vector([1,0]).angle(Vector([0,2]))
        1.5707963267948966
        """
        try:
            return math.acos(v.inner(w) * (1 / (v.magnitude * w.magnitude)))
        except ValueError as e:
            if str(e) == v.NORM_ZERO_ERR_MSG:
                raise ValueError('angle with the zero vector undefined')
            elif v.dimension != w.dimension:
                raise ValueError(
                    'angle undefined on vectors of different dimensions')
            else:
                raise e

    def projected(v, basis):
        """
        v projected onto w

        >>> v = Vector(['1.0', '0.0'], type=Decimal)
        >>> w = Vector(['0.5', '0.5'], type=Decimal)
        >>> v.projected(w)
        Vector(['0.5', '0.5'], type=Decimal)
        """
        return basis * (v.inner(basis) / (basis.magnitude ** 2))

    def is_zero(v):
        return v.magnitude < v.TOLERANCE

    def is_orthogonal(v, w):
        return abs(v.inner(w)) < v.TOLERANCE

    def is_parallel(v, w):
        return (
            v.is_zero() or
            w.is_zero() or
            v.normalized == w.normalized or
            v.normalized * -1 == w.normalized
        )


def area_of_parallelogram(v, w):
    return v.cross(w).magnitude


def area_of_triangle(v, w):
    return 0.5 * area_of_parallelogram(v, w)


def exercises():
    V = Vector

    # Coding cross products
    v = V([8.462, 7.893, -8.187])
    w = V([6.984, -5.975, 4.778])
    print('{} cross {} is {}\n'.format(v, w, v.cross(w)))

    v = V([-8.987, -9.838, 5.031])
    w = V([-4.268, -1.861, -8.866])
    print('area of parallelogram spanned by {} and {} is {}\n'
          .format(v, w, area_of_parallelogram(v, w)))

    v = V([1.5, 9.547, 3.691])
    w = V([-6.007, 0.124, 5.772])
    print('area of triangle spanned by {} and {} is {}\n'
          .format(v, w, area_of_triangle(v, w)))


if __name__ == '__main__':
    exercises()
    import doctest
    doctest.testmod()
