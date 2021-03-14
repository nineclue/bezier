import scala.annotation.tailrec
import collection.mutable.ArrayBuffer

/*
trait BezierAsPoints[A[_]] {
    def knots: A[Point]
    def controls1: A[Point]
    def controls2: A[Point]
}
*/

object Bezier {
    type BezierLayer = Seq[Seq[Point]]
    type BezierAsPoints = (ArrayBuffer[Point], ArrayBuffer[Point], ArrayBuffer[Point])

    /**
      * Calculate points of Bezier curve
      *
      * @param segmentNo : number of points between start and end points
      * @param ps : points, head & last are start & end points, middle are control points
      * @return : points that constitute Bezier curve, including start & end points
      */
    def apply(ps: Seq[Point], segmentNo: Int): Seq[Point] = {
        // reduce to nth point
        @tailrec
        def h(n: Int, pss: Seq[Point]): Point = 
            if (pss.size == 1) pss(0)
            else h(n, pss.sliding(2, 1).map({ case pa => 
                nth(pa(0), pa(1), n, segmentNo)}).toSeq)
        Range(0, segmentNo).toSeq.
            map(i => h(i, ps)).
            prepended(ps.head).appended(ps.last)
    }

    def apply(ps: BezierAsPoints, i: Int, segmentNo: Int = 10): Seq[Point] = 
        apply(Seq(ps._1(i), ps._2(i), ps._3(i), ps._1(i+1)), segmentNo)

    // same as apply, result includes all intermediate calculated points
    def getLayers(ps: Seq[Point], segmentNo: Int = 10): Seq[BezierLayer] = {
        @tailrec
        def h(n: Int, pss: Seq[Point], acc: Seq[Seq[Point]]): BezierLayer = 
            if (pss.size == 1) acc :+ pss
            else {
                val reduced = pss.sliding(2, 1).map({ case pa => 
                                nth(pa(0), pa(1), n, segmentNo)}).toSeq
                h(n, reduced, acc :+ pss)
            }
        Range(0, segmentNo).toSeq.
            map(i => h(i, ps, Seq.empty))
    }

    // nth point along the line between p1 and p2, segmented by total number
    private def nth(p1: Point, p2: Point, n: Int, total: Int): Point = 
        Point((p2.x - p1.x) / total * n + p1.x, ((p2.y - p1.y) / total) * n + p1.y)

    def assemble(ps: BezierAsPoints): Iterator[Bezier] = {
        val (knots, c1s, c2s) = ps
        knots.sliding(2, 1).zipWithIndex.map({ case (pss, i) =>
            Bezier(pss(0), c1s(i), c2s(i), pss(1))
        })
    }

    def deassemble(bs: Seq[Bezier]): BezierAsPoints = ???

    def append(bps: BezierAsPoints, knot: Point): Unit = {
        val (ks, c1s, c2s) = bps
        ks += knot
        ks.length match {
            case 1 => 
            case 2 => 
                val c1 = Point((ks(0).x * 2 + knot.x) / 3, (ks(0).y * 2 + knot.y) / 3)
                val c2 = Point(2 * c1.x - ks(0).x, (2 * c1.y - ks(0).y))
                c1s += c1
                c2s += c2
            case n =>
                val rhs: ArrayBuffer[(Double, Double)] = ArrayBuffer(
                    Range(1, n-1).map(i => (ks(i).x * 4 + ks(i+1).x * 2, ks(i).y * 4 + ks(i+1).y * 2)):_*).
                    prepend((ks(0).x + ks(1).x * 2, ks(0).y + ks(1).y * 2)).
                    append(((ks(n-2).x * 8 + ks(n-1).x) / 2, (ks(n-2).y * 8 + ks(n-1).y) / 2))
                val (rhsx, rhsy) = rhs.unzip
                val xs = calcC1(rhsx)
                val ys = calcC1(rhsy)
                c1s.clear
                c2s.clear
                Range(0, n-1).foreach({ i => 
                    c1s += Point(xs(i), ys(i))
                    val c2 = 
                        if (i < n-1) Point(ks(i+1).x * 2 - xs(i+1), ks(i+1).y * 2 - ys(i+1))
                        else Point((ks(n-1).x + xs(n-2)) / 2, (ks(n-1).y + ys(n-2)) / 2)
                    c2s += c2
                })
        } 
    }
    
    private def calcC1(ds: ArrayBuffer[Double]): ArrayBuffer[Double] = {
        val n = ds.length
        val xs = ArrayBuffer.fill(n)(0.0)
        val ts = ArrayBuffer.fill(n)(0.0)

        var b = 2.0
        xs(0) = ds(0) / b
        Range(1, n).foreach({ i =>
            ts(i) = 1 / b
            b = (if (i < n-1) 4.0 else 3.5) - ts(i)
            xs(i) = (ds(i) - xs(i-1)) / b
        })

        Range(1, n).foreach(i => xs(n-i-1) -= ts(n-i) * xs(n-i))
        xs
    }

    def close(bs: ArrayBuffer[Bezier]): Bezier = ???
}

case class Bezier(var start: Point, var c1: Point, var c2: Point, var end: Point) {  // QuadCurve
    private val segments = 15
    def points = Bezier.apply(Seq(start, c1, c2, end), segments)
    def near(x: Double, y: Double) = 
        Seq(start, c1, c2, end).zipWithIndex.find({ case ((p, i)) => p.near(x, y) }).map(_._2)
    def update(i: Int, x: Double, y: Double) = i match {
        case 0 => start = Point(x, y)
        case 1 => c1 = Point(x, y)
        case 2 => c2 = Point(x, y)
        case 3 => end = Point(x, y)
    }
}

/* 
 * https://www.codeproject.com/Articles/31859/Draw-a-Smooth-Curve-through-a-Set-of-2D-Points-wit

/// <summary>
/// Bezier Spline methods
/// </summary>
public static class BezierSpline
{
	/// <summary>
	/// Get open-ended Bezier Spline Control Points.
	/// </summary>
	/// <param name="knots">Input Knot Bezier spline points.</param>
	/// <param name="firstControlPoints">Output First Control points
	/// array of knots.Length - 1 length.</param>
	/// <param name="secondControlPoints">Output Second Control points
	/// array of knots.Length - 1 length.</param>
	/// <exception cref="ArgumentNullException"><paramref name="knots"/>
	/// parameter must be not null.</exception>
	/// <exception cref="ArgumentException"><paramref name="knots"/>
	/// array must contain at least two points.</exception>
	public static void GetCurveControlPoints(Point[] knots,
		out Point[] firstControlPoints, out Point[] secondControlPoints)
	{
		if (knots == null)
			throw new ArgumentNullException("knots");
		int n = knots.Length - 1;
		if (n < 1)
			throw new ArgumentException
			("At least two knot points required", "knots");
		if (n == 1)
		{ // Special case: Bezier curve should be a straight line.
			firstControlPoints = new Point[1];
			// 3P1 = 2P0 + P3
			firstControlPoints[0].X = (2 * knots[0].X + knots[1].X) / 3;
			firstControlPoints[0].Y = (2 * knots[0].Y + knots[1].Y) / 3;

			secondControlPoints = new Point[1];
			// P2 = 2P1 – P0
			secondControlPoints[0].X = 2 *
				firstControlPoints[0].X - knots[0].X;
			secondControlPoints[0].Y = 2 *
				firstControlPoints[0].Y - knots[0].Y;
			return;
		}

		// Calculate first Bezier control points
		// Right hand side vector
		double[] rhs = new double[n];

		// Set right hand side X values
		for (int i = 1; i < n - 1; ++i)
			rhs[i] = 4 * knots[i].X + 2 * knots[i + 1].X;
		rhs[0] = knots[0].X + 2 * knots[1].X;
		rhs[n - 1] = (8 * knots[n - 1].X + knots[n].X) / 2.0;
		// Get first control points X-values
		double[] x = GetFirstControlPoints(rhs);

		// Set right hand side Y values
		for (int i = 1; i < n - 1; ++i)
			rhs[i] = 4 * knots[i].Y + 2 * knots[i + 1].Y;
		rhs[0] = knots[0].Y + 2 * knots[1].Y;
		rhs[n - 1] = (8 * knots[n - 1].Y + knots[n].Y) / 2.0;
		// Get first control points Y-values
		double[] y = GetFirstControlPoints(rhs);

		// Fill output arrays.
		firstControlPoints = new Point[n];
		secondControlPoints = new Point[n];
		for (int i = 0; i < n; ++i)
		{
			// First control point
			firstControlPoints[i] = new Point(x[i], y[i]);
			// Second control point
			if (i < n - 1)
				secondControlPoints[i] = new Point(2 * knots
					[i + 1].X - x[i + 1], 2 *
					knots[i + 1].Y - y[i + 1]);
			else
				secondControlPoints[i] = new Point((knots
					[n].X + x[n - 1]) / 2,
					(knots[n].Y + y[n - 1]) / 2);
		}
	}

	/// <summary>
	/// Solves a tridiagonal system for one of coordinates (x or y)
	/// of first Bezier control points.
	/// </summary>
	/// <param name="rhs">Right hand side vector.</param>
	/// <returns>Solution vector.</returns>
	private static double[] GetFirstControlPoints(double[] rhs)
	{
		int n = rhs.Length;
		double[] x = new double[n]; // Solution vector.
		double[] tmp = new double[n]; // Temp workspace.

		double b = 2.0;
		x[0] = rhs[0] / b;
		for (int i = 1; i < n; i++) // Decomposition and forward substitution.
		{
			tmp[i] = 1 / b;
			b = (i < n - 1 ? 4.0 : 3.5) - tmp[i];
			x[i] = (rhs[i] - x[i - 1]) / b;
		}
		for (int i = 1; i < n; i++)
			x[n - i - 1] -= tmp[n - i] * x[n - i]; // Backsubstitution.

		return x;
	}
}
*/