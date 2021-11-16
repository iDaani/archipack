import copy
from typing import Optional, Any
import numpy as np
import bpy
from math import atan2, pi, sqrt, factorial
from mathutils import Matrix, Vector
from mathutils.geometry import interpolate_bezier
import json
import logging
logger = logging.getLogger("archipack")


# # #  Based on:
# # #  https:# github.com/pelson/antigrain/blob/master/agg-2.4/src/agg_curves.cpp
RECURSION_LIMIT = 8
FLT_EPSILON = 1.19209290e-7
PATH_DISTANCE_EPSILON = 0.001
# 0.5 deg minimum
CURVE_ANGLE_TOL_EPSILON = 0.008726646259971648

# A cusp is a point on the curve at which curve tangent is discontinuous
M_CUSP_LIMIT = 0




Z = Vector((0, 0, 1))


class Knot:

    __slots__ = ('_p0', '_z', '_last', '_next', 'idx')

    def __str__(self):
        _last, _next = " ", " "
        if self._last is not None:
            _last = self._last.idx
        if self._next is not None:
            _next = self._next.idx
        return "idx:%s last:%s next:%s p0:%s p1:%s" % (self.idx, _last, _next, self._p0, self.p1)

    def __init__(self, co, last=None, after=None) -> None:
        """
        :param co:  coordinate
        :param last: first knot of last segment
        :param after: last knot of this segment
        """
        x, y, z = Vector(co).to_3d()
        self._p0 = Vector((x, y, 0))
        self._z = z

        if last is not None:
            last._next = self

        if after is not None:
            after._last = self

        # setup pointers
        self._last = last
        self._next = after
        self.idx = 0

    @property
    def p3d(self) -> Vector:
        x, y, _ = self._p0
        return Vector((x, y, self._z))

    @property
    def p0(self) -> Vector:
        """
        :return: Vector First knot of current segment
        """
        return self._p0

    @p0.setter
    def p0(self, value: Vector) -> None:
        """
        :param value: Vector location of first knot
        :return:
        """
        x, y, z = Vector(value).to_3d()
        self._p0 = Vector((x, y, 0))
        self._z = z

    @property
    def p1(self) -> Vector:
        """Last knot of segment
        last segment of open line has a fake knot
        in same direction as last segment
        :return: last knot of segment
        """
        if self.has_next:
            # knot somewhere in line
            return self._next._p0
        elif self.has_last:
            # last knot of open line, fake knot in same direction as last segment
            return self._p0 + self._last.v
        # knot alone, should never happen
        return self._p0

    @p1.setter
    def p1(self, value: Vector) -> None:
        """
        :param value: Vector location of last knot
        :return:
        """
        raise NotImplementedError

    @property
    def v(self) -> Vector:
        """
        :return: Vector delta p0 -> p1 of segment
        """
        return self.p1 - self._p0

    @v.setter
    def v(self, value: Vector) -> None:
        """
        :param value:
        :return:
        """
        self.p1 = self._p0 + Vector(value).to_3d()

    @staticmethod
    def _sum(a, b):
        x = a + b
        z = x - a
        y = (a - (x - z)) + (b - z)
        return x, y

    @staticmethod
    def _split(a):
        c = (134217729 * a)  # 2 ** 27 + 1
        x = (c - (c - a))
        y = (a - x)
        return x, y

    def _product(self, a, b):
        x = (a * b)
        a1, a2 = self._split(a)
        b1, b2 = self._split(b)
        y = (a2 * b2 - (((x - a1 * b1) - a2 * b1) - a1 * b2))
        return x, y

    def dot2(self, v0, v1):
        x0, y0, z0 = v0
        x1, y1, z1 = v1
        p, s = self._product(x0, x1)
        h, r = self._product(y0, y1)
        p, q = self._sum(p, h)
        s = s + (q + r)
        h, r = self._product(z0, z1)
        p, q = self._sum(p, h)
        s = s + (q + r)
        return (p + s)

    @staticmethod
    def dot(v0, v1):
        """
        2d dot product
        :param v0:
        :param v1:
        :return:
        """
        return v0.to_2d().dot(v1.to_2d())

    @staticmethod
    def cross(vec):
        """2d cross vector on xy plane
        :param vec:
        :return:
        """
        x, y, z = vec
        return Vector((y, -x, 0))

    def sized_cross(self, vec, length):
        """2d cross vector on xy plane
        :param vec:
        :param length:
        :return:
        """
        return self.cross(vec).normalized() * length

    @property
    def dz(self) -> float:
        _dz = 0
        if self.has_next:
            _dz = self._next._z - self._z
        return _dz

    @property
    def z0(self) -> float:
        return self._z

    @z0.setter
    def z0(self, value: float) -> None:
        self._z = value

    @property
    def z1(self) -> float:
        _z = self._z
        if self.has_next:
            _z = self._next._z
        return _z

    @property
    def v_2d(self):
        return self.v.to_2d()

    @property
    def v_length(self) -> float:
        return self.v.length

    @v_length.setter
    def v_length(self, length: float) -> None:
        self.v = length * self.v_normalized

    @property
    def v_normalized(self) -> Vector:
        return self.v.normalized()

    @property
    def v_cross(self) -> Vector:
        """
        :return: 2d Cross vector (perpendicular to v)
        """
        return self.cross(self.v)

    @property
    def v_normal(self) -> Vector:
        """
        :return: Normalized 2d cross vector (perpendicular to v)
        """
        return self.v_cross.normalized()

    def length_right(self, length: float) -> None:
        """Move p1 so distance p0 -> p1 match length
        :param length: distance p0 -> p1
        :return:
        """
        self.v_length = length

    def length_left(self, length: float) -> None:
        """Move p0 so distance p0 -> p1 match length
        :param length: distance p0 -> p1
        :return:
        """
        _z = self._z
        self.p0 = self.p1 - length * self.v_normalized
        self._z = _z

    @property
    def has_next(self) -> bool:
        """
        :return:
        """
        return self._next is not None

    @property
    def has_last(self) -> bool:
        return self._last is not None

    @property
    def is_first(self) -> bool:
        return self._last is None

    @property
    def is_last(self) -> bool:
        return self._next is None

    @staticmethod
    def angle(vec):
        x, y, z = vec
        return atan2(y, x)

    @property
    def z_angle(self) -> float:
        """Absolute angle of vector p0 -> p1 on z plane
        :return: 2d angle of vector p0 -> p1
        """
        return atan2(self.dz, self.length)

    def delta_angle_z(self, last) -> float:
        if last is None:
            return 0
        u = Vector((last.length, last.dz, 0))
        v = Vector((self.length, self.dz, 0))
        return self.signed_angle(u, v)

    def lerp_z(self, t: float) -> float:
        return self._z + t * self.dz

    @property
    def v_angle(self) -> float:
        """Absolute angle of vector p0 -> p1 on xy plane
        :return: 2d angle of vector p0 -> p1
        """
        return self.angle(self.v)

    @v_angle.setter
    def v_angle(self, angle: float) -> None:
        """Absolute angle of vector p0 -> p1 on xy plane
        :param angle: radians
        :return:
        """
        self.v = Matrix.Rotation(angle, 3, 'Z') @ Vector((self.v_length, 0, 0))

    @staticmethod
    def limit_angle(angle: float) -> float:
        """
        :param angle: radians
        :return: angle radians in range [-pi : pi]
        """
        _a = angle
        if _a > pi:
            _a -= 2 * pi
        if _a < -pi:
            _a += 2 * pi
        return _a

    @property
    def a0(self):
        """2d angle of vector between last and current segment
        :return: float radians
        """
        return self.delta_angle(self._last)

    @property
    def angle_normal(self):
        """
        2d angle of perpendicular
        lie on the right side
        p1
        |--x
        p0
        :return: float radians absolute 2d angle of vector
        """
        x, y, z = self.v
        return atan2(-x, y)

    @staticmethod
    def signed_angle(u: Vector, v: Vector) -> float:
        """
        :param u: Vector 1
        :param v: Vector 2
        :return: radians signed angle between two vectors range [-pi, pi]
        """
        return atan2(u.x * v.y - u.y * v.x, u.x * v.x + u.y * v.y)

    def signed_angle_ext(self, u: Vector, v: Vector, cw: bool) -> float:
        """Signed angle with respect to orientation
        :param u: vector
        :param v: vector
        :param cw: orientation
        :return: float angle in range [2 * pi |-2 * pi]
        """
        a = self.signed_angle(u, v)
        if cw:
            if a > 0:
                a -= 2 * pi
        elif a < 0:
            a += 2 * pi
        return a

    def delta_angle(self, last) -> float:
        """
        :param last: knot derived class
        :return:
            signed delta angle between end of line and start of this one
            this value is object's a0 for segment = self
        """
        if last is None:
            return self.v_angle
        return self.signed_angle(last.v, self.v)

    def steps(self, length: float) -> (float, int):
        """Compute step count given desired step length
        :param length: max space between steps
        :return: param t for step, number of steps
        """
        steps = max(1, round(self.length / length, 0))
        return 1.0 / steps, int(steps)

    def copy(self, last=None):
        """
        :param last: Knot derived
        :return: Knot derived
        """
        _k = copy.copy(self)
        _k._last = last
        if last is not None:
            last._next = _k
        return _k

    def translate(self, dp):
        self._p0 += dp
        if type(self._next).__name__ == 'Knot':
            self.p1 = self.p1 + dp
        return self

    def rotate(self, angle: float):
        """
            Rotate segment ccw arroud p0
        """
        self.v = Matrix.Rotation(angle, 3, 'Z') @ self.v
        return self

    def prerotate(self, rM: Matrix) -> None:
        """
        Rotate knot using a 4x4 matrix
        :param rM:
        :return:
        """
        self._p0 = rM @ self._p0

    def reverse_pointers(self):
        self._next, self._last = self._last, self._next


class Line(Knot):
    """
        2d Line
        Internally stored as p1
        moving p0 or p1 move only one end of line
            p1
            ^
            | v
            p0
    """
    def __init__(self, p0, p1=None, last=None, after=None) -> None:
        """
            :param p0: start point
            :param p1: end point (optional, build standalone segment when set)
            :param last: last segment
            :param after: next line
        """
        Knot.__init__(self, p0, last=last, after=after)

        # build standalone segment
        if p1 is not None:
            Knot(p1, last=self)

    @Knot.p1.setter
    def p1(self, value: Vector) -> None:
        """
        :param value: Vector new location of p1
        :return:
        """
        if self.has_next:
            self._next.p0 = value

    @property
    def length(self) -> float:
        """
        :return: float length of segment
        """
        return self.v_length

    @length.setter
    def length(self, length: float) -> None:
        self.v_length = length

    @property
    def opposite(self):
        """
        :return: standalone segment in opposite direction
        """
        return self.new_segment(self.p1, self.p0)

    def new_segment(self, k0, k1):
        """
        Create standalone segment from k0 and k1
        Override to handle subclass attributes if any
        :param k0:
        :param k1:
        :return: Line
        """
        _k = Line(k0, p1=k1)
        _k._z = self._z
        return _k

    def normal(self, t: float=0, size: float=1.0):
        """
            2d Line perpendicular on plane xy
            at position t in current segment
            lie on the right side when size > 0
            p1
            |--x
            p0
        """
        _p = self.lerp(t)
        _v = size * self.v_normal
        return self.new_segment(_p, _p + _v)

    def lerp(self, t):
        """
            3d interpolation
        """
        return self._p0 + self.v * t

    def intersect(self, line):
        """
            2d intersection on plane xy
            return
            True if intersect
            p: point of intersection
            t: param t of intersection on current line
        """
        c = line.v_cross
        d = self.dot(self.v, c)
        if abs(d) < 0.0001:
            return False, self.p1, 0
        t = self.dot(c, line._p0 - self._p0) / d
        return True, self.lerp(t), t

    def intersect_ext(self, line, side="START"):
        """
            same as intersect, but return param t on both lines
            res is True when intersects
            collinear either False when crossing and None when not crossing
        """
        if hasattr(line, "_r"):
            # Intersect Line / Arc
            it, p, u, v = line.intersect_ext(self, side)
            # reverse v u order !
            return it, p, v, u
        else:
            # Intersect Line / Line
            c = line.v_cross
            d = self.dot(self.v, c)
            if abs(d) < 0.0001:
                res = False
                it, _d, t = self.point_sur_segment(line.p0)
                if abs(_d) > 0.001:
                    # collinear not crossing
                    res = None
                # co-linear consider as outside
                return res, self.p1, 1, 0
            dp = line._p0 - self._p0
            c2 = self.v_cross
            u = self.dot(c, dp) / d
            v = self.dot(c2, dp) / d
            if self.v_length > line.v_length:
                pt = self.lerp(u)
            else:
                pt = line.lerp(v)
            return 1 > u > 0 and 1 > v > 0, pt, u, v

    def point_sur_segment(self, pt):
        """ _point_sur_segment
            point: Vector 2d
            t: param t de l'intersection sur le segment courant
            d: distance laterale perpendiculaire positif a droite
        """
        dp = pt - self._p0
        dl = self.v_length
        if dl == 0:
            return dp.length < 0.0001, 0 ,0
        x, y, z = self.v
        d = (x * dp.y - y * dp.x) / dl
        t = self.dot(self.v, dp) / (dl * dl)
        return 1 >= t >= 0, d, t

    def offset(self, offset, idx=None):
        """
        :param offset:
        :return: standalone segment with offset
        """
        _v = offset * self.v_normal
        _k = self.new_segment(self._p0 + _v, self.p1 + _v)
        _k.idx = self.idx
        return _k

    def tangeant(self, t, da, radius):
        _p = self.lerp(t)
        return Arc(_p, radius, da)

    def straight(self, length: float, t: float=1):
        """
        :param length:
        :param t:
        :return: Tangeant LineSegment
        """
        _p = self.lerp(t)
        _v = self.v_normalized * length
        return self.new_segment(_p, _p + _v)

    def tangeant_unit_vector(self, t: float, length: float=1.0):
        return length * self.v_normalized

    def pts(self, verts):
        verts.append(self._p0)

    def reverse(self):
        # self._p0 = self.p1.copy()
        return self


class Arc(Knot):
    """
        Represent a 3d Arc
        using start/end p0 and p1, radius and angle
    """

    __slots__ = ('_r', '_da', '_c')

    def __init__(self, co: Vector, radius: float, da: float, p1: Vector=None, last=None, after=None) -> None:
        """
        :param co: coordinate of first knot
        :param radius:
        :param da: radians delta angle from start to end CCW when > 0
        :param p1: optional coordinate of last knot
        :param last: first knot of last segment
        :param after: last knot of this segment
        """
        Knot.__init__(self, co, last=last, after=after)
        self._r = radius
        self._da = da

        # cache for center
        self._c = None

        logger.debug('Arc.init() radius:%.2f  da:%.2f', radius, da)
        # setup a standalone segment using p1
        if p1 is not None:
            Knot(p1, last=self)

    @property
    def cw(self) -> bool:
        """
        :return: True when arc is CW
        """
        return self._da <= 0

    @property
    def ccw(self) -> bool:
        """
        :return: True when arc is CCW
        """
        return self._da > 0

    @property
    def c(self) -> Vector:
        """Center of arc
        :return: coordinate of center
        """
        if self._c is None:

            _b2 = self.r2 - (0.5 * self.v_length) ** 2
            _b = 0

            if _b2 > 0:
                # b2 -> squared error
                _b = sqrt(round(_b2, 6))

            _c = _b * self.v_normal

            if self.ccw:
                _c = -_c

            self._c = self._p0 + 0.5 * self.v + _c
            logger.debug("Arc.c() c:%s b2:%s b:%s", self._c, _b2, _b)

        return self._c

    @property
    def c0(self):
        """
        :return: normalized vector c -> p0
        """
        return (self.p0 - self.c).normalized()

    @property
    def c1(self):
        """
        :return: normalized vector c -> p1
        """
        return (self.p1 - self.c).normalized()

    @property
    def r2(self) -> float:
        """
        :return: squared radius
        """
        return self._r ** 2

    @Knot.p0.setter
    def p0(self, value: Vector) -> None:
        """Scale arc radius so it intersects p0 p1
        :param value: Vector, location of new p0
        :return:
        """
        x, y, z = Vector(value).to_3d()
        _co = Vector((x, y, 0))
        _vl = self.v_length
        # invalidate center
        self._c = None
        self._p0 = _co
        self._z = z
        if _vl != 0:
            self._r *= self.v_length / _vl

    @Knot.p1.setter
    def p1(self, value: Vector) -> None:
        """Scale arc so it intersects p0 p1
        :param value: Vector, location of new p1
        :return:
        """
        _vl = self.v_length
        # invalidate center
        self._c = None
        if self.has_next:
            self._next.p0 = value
        if _vl != 0:
            self._r *= self.v_length / _vl

    @property
    def length(self) -> float:
        """
        :return: float arc length
        """
        return self._r * abs(self.da)

    @length.setter
    def length(self, value: float) -> None:
        """Rescale arc length this will change radius and p1
        :param value:
        :return:
        """
        _sr = value / self.length
        self.v_length *= _sr

    def radius_left(self, value):
        _sr = value / self._r
        self.length_left(self.v_length * _sr)

    def radius_right(self, value):
        _sr = value / self._r
        self.v_length *= _sr

    @property
    def da(self):
        return self._da

    @da.setter
    def da(self, value):
        _p1 = self.lerp(value / self._da)
        logger.debug("Arc.da() current:%s  new:%s  p1:%s new p1:%s", self._da, value, self.p1, _p1)
        self._da = value
        if self.has_next:
            self._next.p0 = _p1

    def new_segment(self, k0, k1, radius: float, da: float):
        """
        :param k0:
        :param k1:
        :param radius:
        :param da:
        :return: standalone Arc
        """
        _k = Arc(k0, radius, da, p1=k1)
        _k._z = self._z
        return _k

    @property
    def opposite(self):
        """
        :return: standalone Arc, with opposite direction
        """
        return self.new_segment(self.p1, self.p0, self._r, -self._da)

    def normal(self, t: float=0, size: float=1.0) -> Line:
        """
        :param t: float [0:1] parameter from p0 to p1
        :return: Perpendicular Line starting at t
                 always on the right side
        """
        _p = self.lerp(t)
        _n = size * (_p - self.c).normalized()
        if self.cw:
            _n = -_n
        return Line.new_segment(self, _p, _p + _n)

    @property
    def c0_angle(self):
        """
        :return: absolute angle of vector c -> p0
        """
        return self.angle(self.c0)

    @property
    def c1_angle(self):
        """
        :return: absolute angle of vector c -> p0
        """
        return self.angle(self.c1)

    def lerp(self, t: float) -> Vector:
        """Interpolate along segment
        :param t: parameter [0, 1] where 0 is start of arc and 1 is end
        :return: Vector location of interpolated point
        """
        a = self.c0_angle + t * self.da
        return self.c + Matrix.Rotation(a, 3, 'Z') @ Vector((self._r, 0, 0))

    def intersect_ext(self, line, side: str='END', enlarge: bool=True) -> (bool, Vector, float, float):
        """Compute intersection between a Line derived class and this Arc
        :param line: Knot derived class
        :param side: END -> Arc/Line START -> Line/Arc
        :param enlarge:
        :return: Bool when intersect occurs on segment
        """
        _it, _p, _u, _v = True, Vector(), 0, 0

        # ARC ARC intersection
        if hasattr(line, "_r"):
            _r0, _r1 = self.r2, line.r2
            _c0, _c1 = self.c, line.c

            _v = _c1 - _c0
            _d = _v.length

            if _d < 0.00001:
                _it = False
                _u = 1
                _v = 0
                _p = self.p1

            else:
                _a = (_r0 - _r1 + _d * _d) / (2 * _d)
                if _a >= self._r:
                    _h = 0
                else:
                    _a2 = _a * _a
                    # prevent math domain error
                    if _a2 < _r0:
                        _h = sqrt(_r0 - _a2)
                    else:
                        _h = 0

                x, y, z = _v
                _vc = _h / _d * Vector((y, -x, z))

                if self.cw:
                    _vc = -_vc

                _p2 = _c0 + (_a / _d * _v)
                _p = _p2 + _vc
                _it = True
                _u = self.get_t_near_point(_p)
                _v = line.get_t_near_point(_p)

        # Line/Arc, Arc/Line
        else:
            _c = self.c
            v = line.p0 - _c
            a = self.dot(line.v, line.v)    # line.v.dot(line.v)
            b = 2 * self.dot(v, line.v)     #v.dot(line.v)
            c = self.dot(v, v) - self.r2    #v.dot(v) - self.r2
            d = (b * b) - (4 * a * c)
            if a <= 0.0000001 or d < 0:
                # doesnt intersect, find closest point of line
                res, d, _v = line.point_sur_segment(_c)
                _p = line.lerp(_v)
                res, d, _u = self.point_sur_segment(_p)
                _it = False
                print("No intersection", _u, _v, a, d)

            elif d == 0:
                # intersect once either on top or bottom
                _v = -b / (2 * a)
                _p = line.lerp(_v)
                res, d, _u = self.point_sur_segment(_p)
                print("Intersect top or bottom", _u, _v, a, d)

            else:
                aa = 2 * a
                dsq = sqrt(d)
                t0 = (-b + dsq) / aa
                t1 = (-b - dsq) / aa

                p0 = line.lerp(t0)
                p1 = line.lerp(t1)
                t_off = 0.0

                if side == 'END':
                    # line is on end side
                    # u or v are near 1 find closest of 1
                    u = self.get_t_near_end_point(p0)
                    v = self.get_t_near_end_point(p1)
                    t_off = 1.0

                elif side == 'START':
                    # line is on "start side"
                    # u or v are near 0 find closest of 0
                    u = self.get_t_near_start_point(p0)
                    v = self.get_t_near_start_point(p1)

                else:
                    # u or v are on arc find first one on arc
                    u = self.get_t_near_point(p0)
                    v = self.get_t_near_point(p1)
                    _it = 1 > v > 0 or 1 > u > 0

                if abs(u) < abs(v):
                    _p, _u, _v = p0, t_off + u, t0
                else:
                    _p, _u, _v = p1, t_off + v, t1

                logger.debug("Arc.intersect_ext(%s) u:%.8f v:%.8f t0:%.8f t1:%.8f a:%.8f d:%.8f", side, u, v, t0, t1, a, d)

        return _it, _p, _u, _v

    def intersect(self, line) -> (bool, Vector, float):
        """Compute intersection between line and this arc
        :param line: Line derived class
        :return: Bool when intersect occurs on segment
        """
        res, p, u, v = self.intersect_ext(line, side="INSIDE")
        return res and 1 > u > 0 and 1 > v > 0, p, u

    def offset(self, offset: float):
        """Arc offset
        :param offset: offset > 0 on the right part
        :return: standalone Arc with offset
        """
        _o = offset
        if self.cw:
            _o = -_o
        _r = self._r + _o
        _c = self.c
        _k0 = _c + _r * self.c0
        _k1 = _c + _r * self.c1
        # logger.debug("Arc.offset() ccw:%s _r:%s c0:%s  c1:%s", self.ccw, _r, self.c0, self.c1)
        _k = self.new_segment(_k0, _k1, _r, self._da)
        _k.idx = self.idx
        return _k

    def tangeant(self, t: float, length: float) -> Line:
        """Line tangeant to current line at t location
        :param t: location of tangeant along curve
        :param length: length of segment
        :return: Tangeant line so we are able to chain Circle and lines
            Beware, counterpart on Line does return an Arc !
        """
        _p = self.lerp(t)
        _v = self.sized_cross(_p - self.c, length)
        if self.ccw:
            _v = -_v
        return Line.new_segment(self, _p, _p + _v)

    def tangeant_unit_vector(self, t: float, length: float=1.0) -> Vector:
        """Vector tangeant to current line at t location
        :param t: location of tangeant along curve
        :return: Vector
        """
        _p = self.lerp(t)
        _v = self.sized_cross(_p - self.c, length)
        if self.ccw:
            _v = -_v
        return _v

    def straight(self, length: float, t: float=1) -> Line:
        """Segment tangeant to current line at t location
        :param length: length of segment
        :param t: location of tangeant along curve
        :return: Straight Line
        """
        return self.tangeant(t, length)

    def get_t_near_point(self, pt):
        if self._da == 0:
            t = 0
        else:
            _dp = pt - self.c
            a = self.signed_angle_ext(self.c0, _dp, self.cw)
            t = a / self._da
        return t

    def get_t_near_end_point(self, pt):
        """
        :param pt:
        :return: param t of point on arc
        """
        if self._da == 0:
            t = 0
        else:
            _dp = pt - self.c
            a = self.signed_angle(_dp, self.c1)
            t = a / self._da
        return t

    def get_t_near_start_point(self, pt):
        """
        :param pt:
        :return: param t of point on arc
        """
        if self._da == 0:
            t = 0
        else:
            _dp = pt - self.c
            a = self.signed_angle(self.c0, _dp)
            t = a / self._da
        return t

    def point_sur_segment(self, pt: Vector) -> (bool, float, float):
        """Point pt lie on arc ?
        :param pt: Vector point to check
        :return: bool, float, float
        True when pt lie on segment
        t [0, 1] where it lie (normalized between start and end)
        d distance from arc
        """
        t = self.get_t_near_point(pt)
        d = (pt - self.c).length - self._r
        _it = 1 >= t >= 0
        return _it, d, t

    def reverse(self):
        # self._p0 = self.p1
        self._da = -self._da
        return self

    # DEBUG
    def pts(self, verts: list) -> None:
        n_pts = max(1, int(round(abs(self._da) / pi * 30, 0)))
        t_step = 1 / n_pts
        verts.extend([self.lerp(i * t_step) for i in range(n_pts)])


class Generator:
    """
    2.5d Line defined by knots + z
    Implicitly closed by setting last segment _next pointer
    Support line and arc segments
    This class does handle geometric operations and
    provide ways to update objects data model
    # TODO: handle indide raycast method
    """
    __slots__ = ('segs', 'rot', 'location')

    def __init__(self, tM=None) -> None:
        """
        :param tM: Matrix, transform matrix of local space
        """
        self.segs = []
        self.location = Vector()
        if tM is None:
            # delta angle between local and world absolute
            self.rot = Matrix.Rotation(0, 3, 'Z')
        else:
            if hasattr(tM, "matrix_world"):
                _tM = tM.matrix_world
            else:
                _tM = tM
            loc, rot, scale = _tM.decompose()
            self.rot = rot.to_matrix()
            self.location[0:2] = loc[0:2]

    @property
    def tM(self):
        _tM = self.rot.to_4x4()
        _tM.translation = self.location.copy()
        return _tM

    @property
    def itM(self):
        return self.tM.inverted()

    def new_segment(self, k0, k1):
        return Line(k0, p1=k1)

    @property
    def a0(self) -> float:
        """
        :return: angle of first segment in local space
        """
        x, y, z = self.rot.inverted() @ self.segs[0].v
        return atan2(y, x)

    @property
    def hassegs(self) -> bool:
        """
        :return: Boolean, True when curve contains knots
        """
        return self.numknots > 0

    @property
    def origin(self) -> Vector:
        """
        :return: Vector, location of first knot in local space
        """
        _origin = None
        if self.hassegs:
            _origin = self.segs[0].p0
        else:
            _origin = Vector()
        return _origin

    @property
    def numknots(self) -> int:
        """Number of knots
        :return: int number of knots
        """
        return len(self.segs)

    @property
    def numsegs(self) -> int:
        """Number of segments
        :return: int Number of segments
        """
        if self.closed:
            _numsegs = self.numknots
        else:
            _numsegs = self.numknots - 1
        return _numsegs

    @property
    def closed(self) -> bool:
        """Spline is closed
        :return: Bool True when closed
        """
        return self.hassegs and self.segs[0].has_last

    @property
    def last_seg(self) -> Knot:
        """
        :return: Knot derived class last segment first knot
        """
        _k = None
        if self.hassegs:
            _k = self.segs[-1]
        return _k

    def close(self, closed: bool) -> None:
        """Setup pointers between start and end of line
        :return:
        """
        if self.hassegs:
            _k0 = self.segs[0]
            _k1 = self.segs[-1]
            if closed:
                _k0._last = _k1
                _k1._next = _k0
            else:
                _k0._last = None
                # prevent pointer cleaning on segs with knot as next
                if _k1._next and _k1._next.__class__.__name__ != 'Knot':
                    _k1._next = None

    def _move(self, start: int, end: int, dp: Vector) -> None:
        """Move knots between start and end
        :param start: First knot index
        :param end: Last knot index
        :param dp: Vector translation
        :return:
        """
        _segs = self.segs
        for i in range(start, end):
            z = _segs[i]._z
            _segs[i].translate(dp)

    def _move_next(self, idx: int, dp: Vector) -> None:
        """Move from first point of idx till line end
        :param idx: Knot index
        :param dp: Vector translation
        :return:
        """
        self._move(idx, self.numknots, dp)

    def _move_prev(self, idx: int, dp: Vector) -> None:
        """Move previous part of line
        :param idx: Knot index
        :param dp: Vector translation
        :return: Vector translation
        """
        self._move(0, idx, dp)

    def reset_origin(self) -> Vector:
        """Reset line origin to 0,0 by moving all knots
        :return: Vector last origin to move object origin according
        """
        _origin = self.origin.copy() # need a copy
        # if _origin.length > 0.00001:
        self._move(0, self.numknots, -_origin)

        return _origin

    def move_p0(self, idx: int, dp: Vector) -> None:
        """Move first knot of segment
        :param idx: Knot index
        :param dp: delta location
        :return:  Vector translation
        """
        self.segs[idx].p0 = self.segs[idx]._p0 + dp

    def move_p1(self, idx: int, dp: Vector) -> None:
        """Move last knot of segment
        :param idx: Knot index
        :param dp: Vector translation
        :return:
        """
        self.segs[idx].p1 = self.segs[idx].p1 + dp

    def _length_left(self, idx: int, length: float) -> None:
        """Set segment length by moving first knot of segment
        :param idx: Knot index
        :param length: Desired distance from knot 1
        """
        self.segs[idx].length_left(length)

    def _length_right(self, idx: int, length: float) -> None:
        """Set segment length by moving last knot of segment
        :param idx: Knot index
        :param length: Desired distance from knot 0
        """
        self.segs[idx].length_right(length)

    def move(self,  idx: int, length: float, side: str, lock: bool) -> None:
        """
        :param idx:  Knot index
        :param length: segment length
        :param side: string in ['LEFT', 'RIGHT']
        :param lock: boolean, move only 1 segment or all segments in direction
        :return:
        """
        if lock:
            s = self.segs[idx]
            dp = s.v_normalized * (length - s.v_length)
            if side == 'LEFT':
                self._move_prev(idx + 1, -dp)
            else:
                self._move_next(idx + 1, dp)
        else:
            if side == 'LEFT':
                self._length_left(idx, length)
            else:
                self._length_right(idx, length)

    def _prerotation_matrix(self, angle: float, pivot: Vector) -> Matrix:
        """Compute a rotation matrix around pivot
        :param angle: rotation radians
        :param pivot: pivot location
        :return:
        """
        _angle = Knot.limit_angle(angle)
        _tM = Matrix.Translation(pivot)
        return _tM @ Matrix.Rotation(_angle, 4, 'Z') @ _tM.inverted()

    def _prerotate(self, start: int, end: int, angle: float, pivot: Vector) -> None:
        """Prerotate knots between start and end indexes around pivot
        :param start: Knot index
        :param end: Knot index
        :param angle: angle radians
        :param pivot: Vector location of pivot point
        :return:
        """
        _rM = self._prerotation_matrix(angle, pivot)
        _segs = self.segs
        for i in range(start, end):
            _segs[i].prerotate(_rM)

    def _rotate_next(self, idx: int, angle: float) -> None:
        """
        Rotate next part of line around p0
        :param idx: Knot index
        :param angle: Angle radians
        :return:
        """
        _k = self.segs[idx]
        self._prerotate(idx + 1, self.numknots, angle, _k.p0)

    def _rotate_prev(self, idx: int, angle: float) -> None:
        """Rotate previous part of line around p0
        :param idx: Knot index
        :param angle: Angle radians
        :return:
        """
        _k = self.segs[idx]
        if _k.is_first:
            # should never occur but check for safety
            return
        _k = _k._last
        self._prerotate(0, idx, -angle, _k.p1)

    def _rotate_right(self, idx: int, angle: float) -> None:
        """Rotate segment around first knot
        compute projection of last knot over next segment
        :param idx: Segment index
        :param angle: Angle radians
        :return:
        """
        _k = self.segs[idx]
        _rM = self._prerotation_matrix(angle, _k.p0)
        _p1 = _rM @ _k.p1
        if _k.has_next and _k._next.has_next and not hasattr(_k._next, "_r"):
            _next = _k._next
            if abs(_next.delta_angle(_k)) < 0.7853981633974483:
                # under 45 deg use projection over normal
                _v = _k.p1 + _k.cross(_next.p1 - _k.p0)
                _next = self.new_segment(_k.p1, _v)
            else:
                _next = self.new_segment(_next.p0,  _next.p1)
            _s = self.new_segment(_k.p0, _p1)
            it, p, u, v = _s.intersect_ext(_next)
            if 10 > u > -10:
                _k.p1 = p
        else:
            _k.p1 = _p1

    def _rotate_left(self, idx: int, angle: float) -> None:
        """Rotate segment before this one around start point of this segment
        compute projection of first knot over last segment
        :param idx: Segment index
        :param angle: Angle radians
        :return:
        """
        _k = self.segs[idx]
        if _k.is_first:
            # should never occur but check for safety
            return
        _k = _k._last
        # use reverse angle as we do measure delta from the right side
        _rM = self._prerotation_matrix(-angle, _k.p1)
        _p0 = _rM @ _k._p0

        if _k.has_last and hasattr(_k._last, "_r"):
            _k._last.p1 = _p0
        elif _k.is_first:
            _k.p0 = _p0
        else:
            _last = _k._last
            if abs(_last.delta_angle(_k)) < 0.7853981633974483:
                # under 45 deg use projection over normal
                _v = _k.p0 + _k.cross(_k.p1 - _last.p0)
                _last = self.new_segment(_k.p0, _v)
            else:
                _last = self.new_segment(_last.p0, _last.p1)
            _s = self.new_segment(_p0, _k.p1)
            it, p, u, v = _last.intersect_ext(_s)
            if 10 > u > -10:
                _k.p0 = p

    def _bound_seg(self, _k, pts, itM):
        if hasattr(_k, "_r"):
            c = _k._c
            r = _k._r
            pts.append(itM @ Vector((c.x - r, c.y - r, 0)))
            pts.append(itM @ Vector((c.x + r, c.y + r, 0)))
        else:
            pts.append(itM @ _k.p0)

    def bounding_rect(self, itM=None):
        """Bounding rect approximation
        :return:
        """
        if itM is None:
            _itM = Matrix()
        else:
            _itM = itM
        pts = []
        for _k in self.valid_segs:
            self._bound_seg(_k, pts, _itM)
        if not self.closed:
            self._bound_seg(self.segs[-1], pts, _itM)
        x, y, z = zip(*pts)
        return min(x), min(y), max(x), max(y)

    def rotate(self, idx: int, angle: float, side: str, lock: bool) -> None:
        """
        :param idx:  Knot index
        :param angle: Angle radians
        :param side: string in ['LEFT', 'RIGHT']
        :param lock: boolean, rotate only 1 segment or all segments in direction
        :return:
        """
        if lock:
            if side == 'LEFT':
                self._rotate_prev(idx, angle)
            else:
                self._rotate_next(idx, angle)
        else:
            if side == 'LEFT':
                self._rotate_left(idx, angle)
            else:
                self._rotate_right(idx, angle)

    def make_offset(self, offset: float):
        """Create standalone line offset  [+ right : - left]
        :param offset: float lateral distance
        :return: Generator
        """
        line = self.__class__(self.tM)

        if self.hassegs:

            _segs = self.valid_segs
            _k = _segs[0]
            _s0 = None

            if _k.has_last:
                _s0 = _k._last.offset(offset)

            for _k in _segs:

                _s1 = _k.offset(offset)

                _s1.idx = _k.idx

                _p0 = _s1.p0

                if _k.has_last:

                    # on collinear not crossing, p0 is _s0.p1
                    it, _p0, u, v = _s0.intersect_ext(_s1)

                    if it:

                        if hasattr(_s0, "_r"):
                            # recompute da
                            _c1 = _p0 - _s0.c
                            # allow > +-180
                            _da = (_s0.angle(_c1) - _s0.c0_angle) % (2 * pi)
                            # _da = _s0.signed_angle(_s0.c0, _c1)
                            # preserve cw / ccw
                            if (_s0.cw and _da > 0) or (_s0.ccw and _da < 0):
                                _da = -_da
                            _s0._da = _da

                        if hasattr(_s1, "_r"):
                            # recompute da
                            _c0 = _p0 - _s1.c
                            _da = (_s1.c1_angle - _s1.angle(_c0)) % (2 * pi)
                            # _da = _s1.signed_angle(_c0, _s1.c1)
                            # preserve cw / ccw
                            if (_s1.cw and _da > 0) or (_s1.ccw and _da < 0):
                                _da = -_da
                            _s1._da = _da

                _s1._p0 = _p0
                _s1._last = _s0
                if _s0 is not None:
                    _s0._next = _s1
                line.segs.append(_s1)
                _s0 = _s1

            if not self.closed:
                # never check intersection on last invalid segment
                _s1 = self.new_segment(_s0.p1, None)
                _s1.idx = _s0.idx
                _s1._last = _s0
                _s0._next = _s1
                line.segs.append(_s1)

            line.close(self.closed)

        return line

    def make_offset_ext(self, d, offset: float):
        """Create standalone line offset  [+ right : - left] using individual segment offset
        :param d: archipack_segments derived datablock
        :param offset: float lateral distance
        :return: Generator
        """
        line = self.__class__(self.tM)

        if self.hassegs:

            _segs = self.segs
            _parts = d.parts
            _k = _segs[0]
            _p_last = _parts[-1]
            _s0 = None

            if _k.has_last:
                _s0 = _k._last.offset(_p_last.offset + offset)

            for _k, _p in zip(_segs, _parts):

                _s1 = _k.offset(_p.offset + offset)

                _p0 = _s1.p0

                if _k.has_last:

                    it, _p0, u, v = _s0.intersect_ext(_s1)

                    if it:

                        # _p0 = p

                        if hasattr(_s0, "_r"):
                            # recompute da
                            _c1 = _p0 - _s0.c
                            # allow > +-180
                            _da = (_s0.angle(_c1) - _s0.c0_angle) % (2 * pi)
                            # preserve cw / ccw
                            if (_s0.cw and _da > 0) or (_s0.ccw and _da < 0):
                                _da = -_da
                            _s0._da = _da

                        if hasattr(_s1, "_r"):
                            # recompute da
                            _c0 = _p0 - _s1.c
                            _da = (_s1.c1_angle - _s1.angle(_c0)) % (2 * pi)
                            # preserve cw / ccw
                            if (_s1.cw and _da > 0) or (_s1.ccw and _da < 0):
                                _da = -_da
                            _s1._da = _da

                _s1._p0 = _p0
                _s1._last = _s0
                if _s0 is not None:
                    _s0._next = _s1
                line.segs.append(_s1)
                _s0 = _s1

            line.close(self.closed)

        return line

    def set_offset(self, offset: float) -> None:
        """In place offset of line, preserve segments parameters
        :param offset: float lateral distance
        :return:
        """
        if offset != 0.0:
            line = self.make_offset(offset)
            for _src, _dest in zip(line.segs, self.segs):
                _dest._p0 = _src._p0
                if hasattr(_src, "_r"):
                    _dest._r = _src._r
                    _dest._da = _src._da

    def matrix_about_segment(self, idx: int, t:float, offset:float, direction: float=1, altitude: float=0, side: float=1):
        """
        :param idx: segment index
        :param t: t parameter over segment where 0 is start 1 is end
        :param offset: offset from segment on the left side
        :param direction: side of segment normal
        :param side: side of matrix, when -1 flip rotation
        :return: Matrix with x axis in seg direction and y axis on left side when side is 1
        """
        _k = self.segs[idx]
        n = _k.normal(t, direction)
        x, y, z = n.lerp(offset)
        # axis
        xx, xy, _ = side * _k.v_normalized
        yx, yy, _ = side * n.v
        return Matrix([
            [xx, yx, 0, x],
            [xy, yy, 0, y],
            [0, 0, 1, z + altitude],
            [0, 0, 0, 1]
        ])


    def create_segment(self, d, part, co: Vector, last, last_type: str, p1=None, next=None):
        """Add a new segment (Knot derived class)
        TODO: Override this method with Line or Arc inherited sub-classes
        :param d: archipack_segments derived datablock
        :param part: archipack_segment derived instance
        :param co: Vector location of first knot
        :param last: archipack_segment derived instance, last segment
        :param last_type: last part type
        :return:
        """
        if part.type == 0:
            _k = Line(co, last=last)
        else:
            _k = Arc(co, part.radius, part.da, last=last)

        return _k

    def _add_part(self, d, part, last, co: Vector, rM: Matrix, last_type: str, next=None):
        """Append a part to end of curve
        :param d: archipack_segments derived datablock
        :param part: archipack_segment derived instance
        :param co: Vector location of first knot
        :param last: archipack_segment derived instance, last segment
        :param last_type: last part type
        :param next: archipack_segment derived instance, next segment
        :return: Knot, Vector, Matrix, str
        """
        _type = part.type

        # compute location of next knot
        if _type == 0:
            _v = Vector((part.length, 0, 0))
        else:
            # vector p0 p1 length
            _v = Vector((part.radius, 0, 0))
            _v += Matrix.Rotation(part.da, 3, 'Z') @ -_v
            _v = Vector((_v.length, 0, 0))

        _rM = Matrix.Rotation(part.a0, 3, 'Z') @ rM
        _co = co + _rM @ _v

        _k = self.create_segment(d, part, co, last, last_type, p1=_co, next=next)

        _k.idx = self.numknots

        self.segs.append(_k)

        return _k, _co, _rM, _type

    def add_parts(self, d) -> None:
        """Init a generator from archipack_segments datablock
        :param d: archipack_segments derived datablock
        :return:
        """
        _k = None
        _co = self.location
        _rM = self.rot
        _type = 'NONE'
        n_parts = d.num_parts
        for i, part in enumerate(d.parts):
            _k, _co, _rM, _type = self._add_part(d, part, _k, _co, _rM, _type, d.parts[(i + 1) % n_parts])
        self.close(d.is_closed)

    def update_parts(self, d) -> None:
        """Update data from model
        :param d: archipack_segments derived datablock
        :return:
        """
        _parts = d.parts
        for _p, _k in zip(_parts, self.segs):
            _p.a0 = _k.a0
            if hasattr(_k, "_r"):
                _p.type = 1
                _p.radius = _k._r
                _p.da = _k._da
            else:
                _p.type = 0
                _p.length = _k.v_length

        if hasattr(d, 'update_parts_from_generator'):
            d.update_parts_from_generator(self)

        # first angle relative to input coordsys
        d.parts[0].a0 = self.a0

    def get_pts(self, verts):
        verts.extend([s._p0 for s in self.segs])

    def get_verts(self, verts: list) -> None:
        """Fill in given list with vertices coordinates
        :param verts: list to fill
        :return:
        """
        segs = self.segs
        for _k in segs:
            _k.pts(verts)

        # if self.closed:
        #    verts.append(segs[0]._p0)

    @staticmethod
    def pts_as_curve(coords, name, closed, dimensions="3D"):
        curve = bpy.data.curves.new("{}-{}".format(name, dimensions.lower()), type='CURVE')
        curve.dimensions = dimensions

        for pts in coords:
            spline = curve.splines.new('POLY')
            spline.use_endpoint_u = False
            spline.use_cyclic_u = closed
            spline.points.add(len(pts) - 1)
            for p, co in zip(spline.points, pts):
                p.co = co.to_4d()

        curve_obj = bpy.data.objects.new("{}-{}".format(name, dimensions.lower()), curve)
        return curve_obj

    def as_curve(self, name="Curve"):
        """Build a blender curve from data doesnt link to scene
        :param context: Blender context
        return: Blender curve object
        """
        pts = []
        Generator.get_verts(self, pts)
        return self.pts_as_curve([pts], name, self.closed, dimensions="2D")

    def as_curve3d(self, name="Curve"):
        """Build a blender curve from data doesnt link to scene
        :param context: Blender context
        return: Blender curve object
        """
        pts = []

        for _k in self.segs:
            if hasattr(_k, '_r'):
                n_pts = max(1, int(round(abs(_k._da) / pi * 30, 0)))
                t_step = 1 / n_pts
                for i in range(n_pts):
                    x, y, z = _k.lerp(i * t_step)
                    z = _k.lerp_z(i * t_step)
                    pts.append(Vector((x, y, z, 1)))
            else:
                p = _k.p0.to_4d()
                p.z = _k._z
                pts.append(p)
        return self.pts_as_curve([pts], name, self.closed, dimensions="3D")

    def locate_manipulators(self, d, side=1):
        """
            setup manipulators
        """
        _segs = self.segs
        _parts = d.parts

        n_parts = d.num_parts

        for i, part in enumerate(_parts):
            manipulators = part.manipulators
            f = _segs[i]

            p0 = f.p0
            p1 = f.p1

            if i < n_parts:
                # angle from last to current segment
                # when closed use a DUMB_ANGLE for i = 0
                v0 = _segs[i - 1].v_normalized * -side
                v1 = f.v_normalized * side
                # manipulators[0].type_key = 'DUAL_ANGLE'
                # manipulators[0].prop1_name = json.dumps({"angle": "a_ui", "dir": "change_side", "lock": "lock"})
                manipulators[0].prop2_name = json.dumps({"flip": side == -1})
                dist = 1
                if abs(part.a0) < 0.4:
                    dist = abs(side) * 0.8
                manipulators[0].set_pts([p0, dist * v0, dist * v1])

                if part.type == 0:
                    # segment length
                    manipulators[1].set_pts([p0, p1, (side, 0, 0)])
                else:
                    # segment radius + angle
                    v0 = part.radius * f.c0
                    v1 = part.radius * f.c1
                    manipulators[1].set_pts([f.c, v0, v1])

                # snap manipulator, dont change index !
                manipulators[2].set_pts([p0, p1, (side, 0, 0)])

                # dumb segment id
                manipulators[3].set_pts([p0, p1, (side, 0, 0)])

            else:
                manipulators[2].set_pts([p0, _segs[i - 1].p0, (side, 0, 0)])

            """
            # offset (currently not in use)
            manipulators[4].set_pts([
                p0,
                p0 + f.sized_normal(0, max(0.0001, self.parts[i].offset)).v.to_3d(),
                (0.5, 0, 0)
            ])
            """
            # Dimensions points
            d.add_dimension_point(part.uid, f.p0)

            # if not d.is_closed and i == n_parts:
            #    d.add_dimension_point(part.uid + 1, f.p1)

    def make_first(self, idx):
        """Make segment with index idx first one
        :param idx: index of first point
        :return:
        """
        if self.closed:
            _segs = self.segs
            self.segs = _segs[idx:len(_segs)] + _segs[0:idx]
        return self

    def reverse(self):
        # Store p0 for each seg
        # _idx = [s.idx for s in reversed(self.segs)]
        _pts = [s.p1 for s in reversed(self.segs)]
        _segs = [s.reverse() for s in reversed(self.segs)]
        if not self.closed:
            # _pts = .-x _2x _1x _0x
            # _pts = x2_ x1_ x0_ .x-
            _pts.append(_pts.pop(0))
            _segs.append(_segs.pop(0))
            # _idx.append(_idx.pop(0))
            _pts[-1] = self.segs[0]._p0
            _segs[-1]._next = self.segs[0]
            _segs[-1]._last = None
            _segs[0]._next = None
            self.segs[0]._last = _segs[-1]

        for p, s in zip(_pts, _segs):
            s._p0 = p

        for s in _segs:
            s.reverse_pointers()

        # for i, s in enumerate(_segs):
        #    s.idx = _idx[i]

        # print("Generator.reverse() len self.segs:", len(self.segs), " _segs:", len(_segs), "closed:", self.closed)

        self.segs = _segs
        return self

    @property
    def area(self):
        _pts = [_k._p0 for _k in self.segs]
        p0 = _pts[-1]
        d = 0
        for p1 in _pts:
            d += (p1.x * p0.y - p1.y * p0.x)
            p0 = p1
        return 0.5 * abs(d)

    @property
    def center(self):
        """2d center of bounding rectangle
        :return: Vector
        """
        x_min, y_min, x_max, y_max = self.bounding_rect()
        return  0.5 * Vector((x_min + x_max, y_min + y_max, 0))

    @property
    def is_cw(self):
        _pts = [_k._p0 for _k in self.segs]
        p0 = _pts[-1]
        d = 0
        for p1 in _pts:
            d += (p1.x * p0.y - p1.y * p0.x)
            p0 = p1
        return d > 0

    @property
    def is_ccw(self):
        return not self.is_cw

    @property
    def valid_segs(self):
        """Return only valid segs
        :return: array of segs
        """
        _segs = self.segs[:]
        if not self.closed:
            _segs.pop()
        return _segs

    def from_points(self, pts, ccw, cw, reverse, closed):
        _k = None
        _segs = self.segs
        _p = pts[-1]
        for p in pts:
            if (p - _p).length > 0.001:
                _p = p
                _k = Line(_p, last=_k)
                _segs.append(_k)

        self.close(closed)

        if ccw or cw:
            is_cw = self.is_cw

            if reverse:
                is_cw = not is_cw

            if (ccw and is_cw) or (cw and not is_cw):
                self.reverse()

        elif reverse:
            self.reverse()

        # ensure first point is at 0,0
        self.reset_origin()
    
    def _add_curve_point(self, p, last_p, last):
        if (last_p - p).length > 0.001:
            _p = p
            _k = Line(_p, last=last)
            self.segs.append(_k)
        else:
            _p = last_p
            _k = last
        return _k, _p
    
    def interpolate_bezier(self, last, wM, last_p, p0, p1, resolution):
        """
         Bezier curve approximation
        """
        _segs = self.segs
        _k = last
        _p = last_p
        if (resolution == 0 or
                (p0.handle_right_type == 'VECTOR' and
                 p1.handle_left_type == 'VECTOR')):
            p = wM @ p0.co.to_3d()
            _k, _p = self._add_curve_point(p, _p, _k)
            
        else:
            v = (p1.co - p0.co).normalized()
            d1 = (p0.handle_right - p0.co).normalized()
            d2 = (p1.co - p1.handle_left).normalized()
            if d1 == v and d2 == v:
                p = wM @ p0.co.to_3d()
                _k, _p = self._add_curve_point(p, _p, _k)
                
            else:
                seg = interpolate_bezier(wM @ p0.co,
                                         wM @ p0.handle_right,
                                         wM @ p1.handle_left,
                                         wM @ p1.co,
                                         resolution + 1)
                seg.pop()
                for pt in seg:
                    p = pt.to_3d()
                    _k, _p = self._add_curve_point(p, _p, _k)

        return _k, _p

    def _adaptive_bezier(self, last, last_p, start, c1, c2, end, level, deviation=0.2, angle_max=0.13962634015954636):
        if level > RECURSION_LIMIT:
            return last, last_p

        _k = last
        _p = last_p

        #  Calculate all the mid-points of the line segments
        # ----------------------
        p12 = 0.5 * (start + c1)
        p23 = 0.5 * (c1 + c2)
        p34 = 0.5 * (c2 + end)
        p123 = 0.5 * (p12 + p23)
        p234 = 0.5 * (p23 + p34)
        p1234 = 0.5 * (p123 + p234)

        if level > 0:
            #  Enforce subdivision first time
            #  Try to approximate the full cubic curve by a single straight line
            # ------------------
            d1 = end - start
            d2 = c1 - end
            d3 = c2 - end

            dl = d1.x ** 2 + d1.y ** 2
            d2 = abs(d2.x * d1.y - d2.y * d1.x)
            d3 = abs(d3.x * d1.y - d3.y * d1.x)

            if d2 > FLT_EPSILON and d3 > FLT_EPSILON:
                #  Regular care
                # -----------------
                if (d2 + d3) ** 2 <= deviation * dl:
                    #  If the curvature doesn't exceed the deviation value
                    #  we tend to finish subdivisions.
                    # ----------------------
                    if angle_max < CURVE_ANGLE_TOL_EPSILON:
                        return self._add_curve_point(p1234, _p, _k)

                    #  Angle & Cusp Condition
                    # ----------------------
                    d1 = (c1 - start)
                    d2 = (c2 - c1)
                    d3 = (end - c2)

                    a23 = atan2(d2.y, d2.x)
                    da1 = abs(a23 - atan2(d1.y, d1.x))
                    da2 = abs(atan2(d3.y, d3.x) - a23)
                    if da1 >= pi:
                        da1 = 2 * pi - da1
                    if da2 >= pi:
                        da2 = 2 * pi - da2

                    if da1 + da2 < angle_max:
                        #  Finally we can stop the recursion
                        # ----------------------
                        return self._add_curve_point(p1234, _p, _k)

                    if M_CUSP_LIMIT != 0.0:
                        if da1 > M_CUSP_LIMIT:
                            return self._add_curve_point(c1, _p, _k)

                        if da2 > M_CUSP_LIMIT:
                            return self._add_curve_point(c2, _p, _k)

            else:
                if d2 > FLT_EPSILON:
                    #  p1,p3,p4 are collinear, p2 is considerable
                    # ----------------------
                    if d2 ** 2 <= deviation * dl:
                        if angle_max < CURVE_ANGLE_TOL_EPSILON:
                            return self._add_curve_point(p1234, _p, _k)

                        d1 = (c1 - start)
                        d2 = (c2 - c1)
                        #  Angle Condition
                        # ----------------------
                        da1 = abs(atan2(d2.y, d2.x) - atan2(d1.y, d1.x))
                        if da1 >= pi:
                            da1 = 2 * pi - da1

                        if da1 < angle_max:
                            _k, _p = self._add_curve_point(c1, _p, _k)
                            return self._add_curve_point(c2, _p, _k)

                        if M_CUSP_LIMIT != 0.0:
                            if da1 > M_CUSP_LIMIT:
                                return self._add_curve_point(c1, _p, _k)

                elif d3 > FLT_EPSILON:
                    #  p1,p2,p4 are collinear, p3 is considerable
                    # ----------------------
                    if d3 ** 2 <= deviation * dl:
                        if angle_max < CURVE_ANGLE_TOL_EPSILON:
                            return self._add_curve_point(p1234, _p, _k)

                        #  Angle Condition
                        # ----------------------
                        d1 = (c2 - c1)
                        d2 = (end - c2)
                        da1 = abs(atan2(d2.y, d2.x) - atan2(d1.y, d1.x))
                        if da1 >= pi:
                            da1 = 2 * pi - da1

                        if da1 < angle_max:
                            _k, _p = self._add_curve_point(c1, _p, _k)
                            return self._add_curve_point(c2, _p, _k)

                        if M_CUSP_LIMIT != 0.0:
                            if da1 > M_CUSP_LIMIT:
                                return self._add_curve_point(c2, _p, _k)

                else:
                    #  Collinear case
                    # -----------------
                    d = p1234 - 0.5 * (start + end)
                    if d.x ** 2 + d.y ** 2 <= deviation:
                        return self._add_curve_point(p1234, _p, _k)

        #  Continue subdivision
        # ----------------------
        _k, _p = self._adaptive_bezier(_k, _p, start, p12, p123, p1234, level + 1 , deviation, angle_max)
        _k, _p = self._adaptive_bezier(_k, _p, p1234, p234, p34, end, level + 1 , deviation, angle_max)
        return _k, _p

    def from_spline(self, curve, spline, resolution, ccw, cw, reverse, closed,
                    adaptive=False, deviation=0.2, angle_max=0.13962634015954636):
        """
        :param curve: Blender curve
        :param spline: Blender spline (bezier or poly)
        :param resolution: bezier resolution
        :param ccw: return points in ccw order
        :param cw: return points in cw order
        :param reverse: reverse seg order
        :param close: force closed spline
        :return:
        """
        _k = None

        _itM = self.itM @ curve.matrix_world
        _segs = self.segs
        if spline.type == 'POLY':
            _p = spline.points[-1].co.to_3d()
            for pt in spline.points:
                p = pt.co.to_3d()
                if (p - _p).length > 0.001:
                    _p = p
                    _k = Line(_itM @ _p, last=_k)
                    _segs.append(_k)

        elif spline.type == 'BEZIER':

            points = spline.bezier_points
            _p = _itM @ points[-1].co.to_3d()

            if adaptive:

                # use bezier adaptive

                p = _itM @ points[0].co.to_3d()
                _k, _p = self._add_curve_point(p, _p, _k)

                for i in range(1, len(points)):
                    p0 = points[i - 1]
                    p1 = points[i]

                    # "vector" type segment do not need subdivision
                    # if p1.handle_left_type != 'VECTOR' or p0.handle_right_type != 'VECTOR':
                    p = _itM @ p1.co.to_3d()
                    _k, _p = self._adaptive_bezier(
                        _k, _p,
                        _itM @ p0.co.to_3d(),
                        _itM @ p0.handle_right.to_3d(),
                        _itM @ p1.handle_left.to_3d(),
                        p, 0, deviation, angle_max)

                    # last point of segment, must check derivation using last segment
                    _k, _p = self._add_curve_point(p, _p, _k)

                if closed:
                    p0 = points[-1]
                    p1 = points[0]
                    _k, _p = self._adaptive_bezier(
                        _k, _p,
                        _itM @ p0.co.to_3d(),
                        _itM @ p0.handle_right.to_3d(),
                        _itM @ p1.handle_left.to_3d(),
                        _itM @ p1.co.to_3d(), 0, deviation, angle_max)
                else:
                    p = _itM @ points[-1].co.to_3d()
                    _k, _p = self._add_curve_point(p, _p, _k)
            else:
                for i in range(1, len(points)):
                    p0 = points[i - 1]
                    p1 = points[i]
                    _k, _p = self.interpolate_bezier(_k, _itM, _p, p0, p1, resolution)

                if closed:
                    p0 = points[-1]
                    p1 = points[0]
                    _k, _p = self.interpolate_bezier(_k, _itM, _p, p0, p1, resolution)

                else:
                    p = _itM @ points[-1].co.to_3d()
                    _k, _p = self._add_curve_point(p, _p, _k)

        self.close(closed)

        if ccw or cw:
            is_cw = self.is_cw

            if reverse:
                is_cw = not is_cw

            if (ccw and is_cw) or (cw and not is_cw):
                self.reverse()

        elif reverse:
            self.reverse()

        # ensure first point is at 0,0
        self.reset_origin()

    def _savitzky_golay_filter(self, ay, window_size, order, deriv=0, rate=1):
        """Savitzky Golay low pass filter
        """
        order_range = range(order + 1)
        half_window = (window_size - 1) // 2
        y = np.asarray(ay)
        # precompute coefficients
        b = np.mat([[k ** i for i in order_range] for k in range(-half_window, half_window + 1)])
        m = np.linalg.pinv(b).A[deriv] * rate ** deriv * factorial(deriv)
        # pad the signal at the extremes with
        # values taken from the signal itself
        firstvals = y[0] - (y[1:half_window + 1][::-1] - y[0])
        lastvals = y[-1] - (y[-half_window - 2:-2][::-1] - y[-1])
        y = np.concatenate((firstvals, y, lastvals))
        return np.convolve(m[::-1], y, mode='valid')

    def smooth_altitude(self, sub_samples=1.0, window_size=23, order=3):
        """ Apply a low pass filter to altitudes
        :param sub_samples: normalized sample spacing
        :param window_size:
        :param order:
        :return:
        """
        _segs = self.valid_segs
        dl = [_k.length for _k in _segs]
        tl = np.sum(dl)

        # absolute distance along edge from start
        dist = 0
        for i, v in enumerate(dl):
            dl[i] = dist
            dist += v
        dl.append(dist)

        # normalized sample basis
        n_samples = int(tl / sub_samples)
        ds = tl / (n_samples - 1)
        samples = [i * ds for i in range(n_samples)]
        # normalized samples along edge
        z = [_k._z for _k in self.segs]
        dz = np.interp(samples, dl, z)
        zhat = self._savitzky_golay_filter(dz, window_size, order)
        # interpolate filtered coords at real one location
        zres = np.interp(dl, samples, zhat)
        zres = np.delete(zres, -1)
        for _k, z in zip(_segs, zres):
            _k._z = z

    def __str__(self):
        return "\n".join([str(s) for s in self.segs])


"""

from archipack28.archipack_generator import Generator

o = C.object
d = o.data.archipack_wall2[0]

g = Generator()
g.add_parts(d)


g.rotate_prev(1, radians(25))
g.rotate_prev(2, -radians(25))

o.location += g.reset_origin()
g.update_parts(d)
d.auto_update = False
g.update_parts(d)
d.auto_update = True


left = g.make_offset(0.2)
right = g.make_offset(-0.2)


g.as_curve(C)

verts = []
g.get_verts(verts)

"""
