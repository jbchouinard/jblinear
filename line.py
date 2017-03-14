from decimal import Decimal, getcontext

from vector import Vector

getcontext().prec = 30


class Line(object):

    def __init__(self, normal_vector=None, constant_term=None):
        self.dimension = 2

        if normal_vector is None:
            all_zeros = ['0']*self.dimension
            normal_vector = Vector(all_zeros, type=Decimal)
        self.normal_vector = normal_vector

        if constant_term is None:
            constant_term = '0'
        self.constant_term = Decimal(constant_term)

        self.set_basepoint()
        self.set_direction_vector()

    def set_basepoint(self):
        n = self.normal_vector
        c = self.constant_term
        basepoint_coords = ['0']*self.dimension

        try:
            initial_index = Line.first_nonzero_index(n)
            initial_coefficient = n[initial_index]
            basepoint_coords[initial_index] = c/initial_coefficient
            self.basepoint = Vector(basepoint_coords, type=Decimal)
        except NoNonZeroElements:
            self.basepoint = None

    def set_direction_vector(self):
        n = self.normal_vector
        self.direction_vector = Vector([-n['x'], n['y']], type=Decimal)

    def includes_point(self, point):
        if not isinstance(point, Vector):
            point = Vector(point, type=Decimal)

        path = point - self.basepoint
        return path.is_parallel(self.direction_vector)

    def __repr__(self):
        return 'Line({}, {})'.format(repr(self.normal_vector), self.constant_term)

    def __str__(self):
        num_decimal_places = 3

        def write_coefficient(coefficient, is_initial_term=False):
            coefficient = round(coefficient, num_decimal_places)
            if coefficient % 1 == 0:
                coefficient = int(coefficient)

            output = ''

            if coefficient < 0:
                output += '-'
            if coefficient > 0 and not is_initial_term:
                output += '+'

            if not is_initial_term:
                output += ' '

            if abs(coefficient) != 1:
                output += '{}'.format(abs(coefficient))

            return output

        n = self.normal_vector

        try:
            initial_index = Line.first_nonzero_index(n)
            terms = [write_coefficient(n[i], is_initial_term=(i==initial_index)) + 'x_{}'.format(i+1)
                     for i in range(self.dimension) if round(n[i], num_decimal_places) != 0]
            output = ' '.join(terms)
        except NoNonZeroElements:
            output = '0'

        constant = round(self.constant_term, num_decimal_places)
        if constant % 1 == 0:
            constant = int(constant)
        output += ' = {}'.format(constant)

        return output

    @staticmethod
    def first_nonzero_index(iterable):
        for k, item in enumerate(iterable):
            if not is_near_zero(item):
                return k
        raise NoNonZeroElements()


class NoNonZeroElements(Exception):
    pass


def is_near_zero(val, eps=1e-10):
    return abs(val) < eps
