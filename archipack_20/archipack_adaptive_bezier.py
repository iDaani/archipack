
import bpy
from numpy import linalg, zeros, array, dot
from mathutils import Vector
# 3d
DIMS = 3


"""
terrain = C.object
from archipack28.archipack_generator import Generator
from archipack28.archipack_adaptive_bezier import Curve, BezierFit
bc = Curve()

def matrix_world(curve, spline, g):
    if spline.type == 'POLY':
        p = spline.points[0]
    elif spline.type == 'BEZIER':
        p = spline.bezier_points[0]
    p = curve.matrix_world @ p.co.to_3d()
    tM = g.tM.copy()
    tM.translation = p
    return tM 

curve = C.object
spline = curve.data.splines[0]
g = Generator(curve)
g.from_spline(curve, spline, 12, False, False, False, spline.use_cyclic_u,
                          adaptive=True, deviation=0.2, angle_max=0.13962634015954636
                          )

curve2 = g.as_curve3d(name="Curve")

C.scene.collection.objects.link(curve2)
curve2.matrix_world = matrix_world(curve, spline, g)
spline2 = curve2.data.splines[0]
curve2.select_set(True)
C.view_layer.objects.active = curve2

bc.project_to_ground(curve2, terrain, "Terrain", vgroup=False)

spline2 = curve2.data.splines[0]
g = Generator(curve2)
g.from_spline(curve2, spline2, 12, False, False, False, spline2.use_cyclic_u,
                          adaptive=True, deviation=0.2, angle_max=0.13962634015954636
                          )
g.smooth_altitude()

curve3 = g.as_curve3d(name="Curve")
curve3.matrix_world = matrix_world(curve2, spline2, g)
C.scene.collection.objects.link(curve3)

pts = []
g.get_pts(pts)
points = np.array(pts)

res = bc.bezier_curve(points, nTimes=25)

g = Generator(curve)
pts = [Vector(p) for p in res.tolist()]
g.from_points(pts, False, False, False, spline2.use_cyclic_u)

curve4 = g.as_curve3d(name="Curve")
curve4.matrix_world = matrix_world(curve, spline, g)
C.scene.collection.objects.link(curve4)


bezier_segs = BezierFit.fit([_k.p3d for _k in g.segs], deviation=0.05, dims=3)

# bezier_segs = BezierFit.fit([_k.p0.to_2d() for _k in g.segs], deviation=0.5, dims=2)

curve_obj = BezierFit.as_curve(bezier_segs, closed=False, name="Fit")
C.scene.collection.objects.link(curve_obj)
curve_obj.matrix_world = matrix_world(curve2, spline2, g)




# approximation bezier pour une courbe avec plusieurs splines

def matrix_world(curve, spline, g):
    if spline.type == 'POLY':
        p = spline.points[0]
    elif spline.type == 'BEZIER':
        p = spline.bezier_points[0]
    p = curve.matrix_world @ p.co.to_3d()
    tM = g.tM.copy()
    tM.translation = p
    return tM 


deviation = 0.5

curve2 = C.object

for spline2 in curve2.data.splines:
    g = Generator(curve2)
    g.from_spline(curve2, spline2, 1, False, False, False, spline2.use_cyclic_u, adaptive=False)
    bezier_segs = BezierFit.fit([_k.p3d for _k in g.segs], deviation=deviation, dims=3)
    curve_obj = BezierFit.as_curve(bezier_segs, closed=False, name="Fit")
    C.scene.collection.objects.link(curve_obj)
    curve_obj.matrix_world = matrix_world(curve2, spline2, g)


"""

class Curve:

    @staticmethod
    def project_to_ground(curve, target, name, vgroup=False):

        mod = curve.modifiers.new("{}-wrap".format(name), 'SHRINKWRAP')
        if vgroup:
            mod.vertex_group = "{}".format(name)
        mod.target = target
        mod.invert_vertex_group = vgroup
        mod.wrap_method = 'PROJECT'
        mod.use_project_x = False
        mod.use_project_y = False
        mod.use_project_z = True
        mod.use_negative_direction = True
        mod.use_positive_direction = True
        bpy.ops.object.modifier_apply(apply_as='DATA', modifier=mod.name)

    def comb(self, N, k):
        N = int(N)
        k = int(k)
        if k > N or N < 0 or k < 0:
            return 0
        M = N + 1
        nterms = min(k, N - k)
        numerator = 1
        denominator = 1
        for j in range(1, nterms + 1):
            numerator *= M - j
            denominator *= j
        return numerator // denominator

    def bernstein_poly(self, i, n, t):
        """
         The Bernstein polynomial of n, i as a function of t
        """
        return self._comb(n, i) * (t ** (n - i)) * (1 - t) ** i

    def bezier_curve(self, points, nTimes=1000):
        """
           Given a set of control points, return the
           bezier curve defined by the control points.
           points should be a list of lists, or list of tuples
           such as [ [1,1],
                     [2,3],
                     [4,5], ..[Xn, Yn] ]
            nTimes is the number of time steps, defaults to 1000
            See http://processingjs.nihongoresources.com/bezierinfo/
        """
        nPoints = len(points)
        xPoints, yPoints, zPoints = points.transpose()
        t = np.linspace(0.0, 1.0, nTimes)
        polynomial_array = np.array([self.bernstein_poly(i, nPoints - 1, t) for i in range(nPoints)])
        x = np.dot(xPoints, polynomial_array)
        y = np.dot(yPoints, polynomial_array)
        z = np.dot(zPoints, polynomial_array)
        return np.array([x, y, z]).transpose()


class BezierFit:
    """ Python implementation of
        Algorithm for Automatically Fitting Digitized Curves
        by Philip J. Schneider
        "Graphics Gems", Academic Press, 1990
    	license MIT
    	https://github.com/volkerp/fitCurves/blob/master/fitCurves.py

    	3d fit from generator:
    	bezier_segs = BezierFit.fit([_k.p3d for _k in g.segs], deviation=0.05)
        res = BezierFit.as_curve(bezier_segs, closed=False, name="Bezier")
    """
    @staticmethod
    def fit(pts, deviation=0.2, dims=3):
        """Fit one (ore more) Bezier curves to a set of points
        :param pts: array of Vector of 3d location of points
        :param deviation: maximum allowed deviation of curve
        :param dims: [2|3] 2d or 3d
        :return: array of bezier segments [(p0.co, p0.handle_right, p1.handle_left, p1.co) ..]
        """
        global DIMS
        DIMS = dims
        fit = BezierFit()
        points = array(pts)
        leftTangent = fit._normalize(points[1] - points[0])
        rightTangent = fit._normalize(points[-2] - points[-1])
        segs = fit._fitCubic(points, leftTangent, rightTangent, deviation)
        return [tuple(Vector(k.tolist()).to_3d() for k in s) for s in segs]

    @staticmethod
    def as_curve(segs, closed=False, name="Fit", dims=3):
        """Output as curve, not linked to scene
        :param segs:
        :param closed:
        :param name:
        :param dims:
        :return:
        """
        d = bpy.data.curves.new("{}-{}d".format(name, dims), type='CURVE')
        d.dimensions = '3D'
        spline = d.splines.new('BEZIER')
        spline.use_endpoint_u = False
        spline.use_cyclic_u = closed
        spline.bezier_points.add(len(segs))

        # TODO: handle closed curves
        # last = segs.pop(len(segs) - 1)
        i = 0
        for k0, t0, t1, k1 in segs:
            spline.bezier_points[i].co = k0
            spline.bezier_points[i].handle_right = t0
            spline.bezier_points[i + 1].co = k1
            spline.bezier_points[i + 1].handle_left = t1
            i += 1

        spline.bezier_points[0].handle_left = segs[0][0]
        spline.bezier_points[-1].handle_right = segs[-1][-1]

        curve_obj = bpy.data.objects.new("{}-{}d".format(name, dims), d)
        return curve_obj

    def _q(self, ctrlPoly, t):
        """Evaluates cubic bezier at t
        :param ctrlPoly:
        :param t:
        :return: point
        """
        return (1.0-t)**3 * ctrlPoly[0] + 3*(1.0-t)**2 * t * ctrlPoly[1] + 3*(1.0-t)* t**2 * ctrlPoly[2] + t**3 * ctrlPoly[3]
    
    def _qprime(self, ctrlPoly, t):
        """Evaluates cubic bezier first derivative at t
        :param ctrlPoly:
        :param t:
        :return: point
        """
        return 3*(1.0-t)**2 * (ctrlPoly[1]-ctrlPoly[0]) + 6*(1.0-t) * t * (ctrlPoly[2]-ctrlPoly[1]) + 3*t**2 * (ctrlPoly[3]-ctrlPoly[2])
    
    def _qprimeprime(self, ctrlPoly, t):
        """Evaluates cubic bezier second derivative at t
        :param ctrlPoly:
        :param t:
        :return: point
        """
        return 6*(1.0-t) * (ctrlPoly[2]-2*ctrlPoly[1]+ctrlPoly[0]) + 6*(t) * (ctrlPoly[3]-2*ctrlPoly[2]+ctrlPoly[1])

    def _fitCubic(self, points, leftTangent, rightTangent, error):
        # Use heuristic if region only has two points in it
        if (len(points) == 2):
            dist = linalg.norm(points[0] - points[1]) / 3.0
            bezCurve = [points[0], points[0] + leftTangent * dist, points[1] + rightTangent * dist, points[1]]
            return [bezCurve]
    
        # Parameterize points, and attempt to fit curve
        u = self._chordLengthParameterize(points)
        bezCurve = self._generateBezier(points, u, leftTangent, rightTangent)
        # Find max deviation of points to fitted curve
        deviation, splitPoint = self._computeMaxError(points, bezCurve, u)
        if deviation < error:
            return [bezCurve]
    
        # If error not too large, try some reparameterization and iteration
        if deviation < error**2:
            for i in range(20):
                uPrime = self._reparameterize(bezCurve, points, u)
                bezCurve = self._generateBezier(points, uPrime, leftTangent, rightTangent)
                deviation, splitPoint = self._computeMaxError(points, bezCurve, uPrime)
                if deviation < error:
                    return [bezCurve]
                u = uPrime
    
        # Fitting failed -- split at max error point and fit recursively
        beziers = []
        centerTangent = self._normalize(points[splitPoint-1] - points[splitPoint+1])
        beziers += self._fitCubic(points[:splitPoint+1], leftTangent, centerTangent, error)
        beziers += self._fitCubic(points[splitPoint:], -centerTangent, rightTangent, error)
    
        return beziers

    def _generateBezier(self, points, parameters, leftTangent, rightTangent):

        bezCurve = [points[0], None, None, points[-1]]
        # compute the A's
        A = zeros((len(parameters), 2, DIMS))
        for i, u in enumerate(parameters):
            A[i][0] = leftTangent  * 3*(1-u)**2 * u
            A[i][1] = rightTangent * 3*(1-u)    * u**2
    
        # Create the C and X matrices
        C = zeros((2, DIMS))
        X = zeros(DIMS)
    
        for i, (point, u) in enumerate(zip(points, parameters)):
            C[0][0] += dot(A[i][0], A[i][0])
            C[0][1] += dot(A[i][0], A[i][1])
            C[1][0] += dot(A[i][0], A[i][1])
            C[1][1] += dot(A[i][1], A[i][1])

            tmp = point - self._q([points[0], points[0], points[-1], points[-1]], u)
    
            X[0] += dot(A[i][0], tmp)
            X[1] += dot(A[i][1], tmp)

        # Compute the determinants of C and X
        det_C0_C1 = C[0][0] * C[1][1] - C[1][0] * C[0][1]
        det_C0_X  = C[0][0] * X[1] - C[1][0] * X[0]
        det_X_C1  = X[0] * C[1][1] - X[1] * C[0][1]
    
        # Finally, derive alpha values
        if det_C0_C1 == 0:
            alpha_l = 0.0
        else:
            alpha_l = det_X_C1 / det_C0_C1

        if det_C0_C1 == 0:
            alpha_r = 0.0
        else:
            alpha_r = det_C0_X / det_C0_C1
    
        # If alpha negative, use the Wu/Barsky heuristic (see text) */
        # (if alpha is 0, you get coincident control points that lead to
        # divide by zero in any subsequent NewtonRaphsonRootFind() call. */
        segLength = linalg.norm(points[0] - points[-1])
        epsilon = 1.0e-6 * segLength
        if alpha_l < epsilon or alpha_r < epsilon:
            # fall back on standard (probably inaccurate) formula, and subdivide further if needed.
            bezCurve[1] = bezCurve[0] + leftTangent * (segLength / 3.0)
            bezCurve[2] = bezCurve[3] + rightTangent * (segLength / 3.0)
    
        else:
            # First and last control points of the Bezier curve are
            # positioned exactly at the first and last data points
            # Control points 1 and 2 are positioned an alpha distance out
            # on the tangent vectors, left and right, respectively
            bezCurve[1] = bezCurve[0] + leftTangent * alpha_l
            bezCurve[2] = bezCurve[3] + rightTangent * alpha_r
    
        return bezCurve

    def _reparameterize(self, bezier, points, parameters):
        return [self._newtonRaphsonRootFind(bezier, point, u) for point, u in zip(points, parameters)]

    def _newtonRaphsonRootFind(self, bez, point, u):
        """
           Newton's root finding algorithm calculates f(x)=0 by reiterating
           x_n+1 = x_n - f(x_n)/f'(x_n)
           We are trying to find curve parameter u for some point p that minimizes
           the distance from that point to the curve. Distance point to curve is d=q(u)-p.
           At minimum distance the point is perpendicular to the curve.
           We are solving
           f = q(u)-p * q'(u) = 0
           with
           f' = q'(u) * q'(u) + q(u)-p * q''(u)
           gives
           u_n+1 = u_n - |q(u_n)-p * q'(u_n)| / |q'(u_n)**2 + q(u_n)-p * q''(u_n)|
        """
        d = self._q(bez, u) - point
        numerator = (d * self._qprime(bez, u)).sum()
        denominator = (self._qprime(bez, u)**2 + d * self._qprimeprime(bez, u)).sum()
    
        if denominator == 0.0:
            return u
        else:
            return u - numerator/denominator

    def _chordLengthParameterize(self, points):
        u = [0.0]
        for i in range(1, len(points)):
            # 3d length
            u.append(u[i-1] + linalg.norm(points[i] - points[i-1]))
    
        for i, _ in enumerate(u):
            u[i] = u[i] / u[-1]
    
        return u

    def _computeMaxError(self, points, bez, parameters):
        maxDist = 0.0
        splitPoint = len(points)/2
        for i, (point, u) in enumerate(zip(points, parameters)):
            dist = linalg.norm(self._q(bez, u)-point)**2
            if dist > maxDist:
                maxDist = dist
                splitPoint = i
    
        return maxDist, splitPoint

    def _normalize(self, v):
        return v / linalg.norm(v)