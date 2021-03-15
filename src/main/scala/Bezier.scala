import scala.annotation.tailrec
import collection.mutable.ArrayBuffer

object Bezier {
    type BezierLayer = Seq[Seq[Point]]
    // type BezierAsPoints = (ArrayBuffer[Point], ArrayBuffer[Point], ArrayBuffer[Point], Boolean)

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

    def apply(ps: BezierSpline, i: Int, segmentNo: Int = 10): Seq[Point] = 
        apply(Seq(ps.knots(i), ps.c1s(i), ps.c2s(i), ps.knots(i+1)), segmentNo)

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

    def assemble(ps: BezierSpline): Iterator[Bezier] = {
        ps.knots.sliding(2, 1).zipWithIndex.map({ case (pss, i) =>
            Bezier(pss(0), ps.c1s(i), ps.c2s(i), pss(1))
        })
    }

    def deassemble(bs: Seq[Bezier]): BezierSpline = ???
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

case class BezierSpline(knots: ArrayBuffer[Point], c1s: ArrayBuffer[Point], c2s: ArrayBuffer[Point], var closed: Boolean = false) {
    def closable() = knots.length >= 2
    def close() = 
        if (closable()) {
            val n = knots.length
            val a = ArrayBuffer.fill(n)(1.0)
            val b = ArrayBuffer.fill(n)(4.0)
            val c = ArrayBuffer.fill(n)(1.0)

            val rhs = ArrayBuffer.fill(n)(0.0)
            Range(0, n).foreach({ i =>
                val j = if (i == n-1) 0 else i+1
                rhs(i) = knots(i).x * 4 + knots(j).x * 2
            })
            val xs = solveCyclic(a, b, c, 1, 1, rhs)

            Range(0, n).foreach({ i =>
                val j = if (i == n-1) 0 else i+1
                rhs(i) = knots(i).y * 4 + knots(j).y * 2
            })
            val ys = solveCyclic(a, b, c, 1, 1, rhs)

            c1s.clear
            c2s.clear
            Range(0, n).foreach({ i =>
                c1s += Point(xs(i), ys(i))
                c2s += Point(2 * knots(i).x - xs(i), 2 * knots(i).y - ys(i))
            })
            closed = true
            knots += knots(0)   // CHECK!
        }

    def append(knot: Point): Unit = {
        knots += knot
        knots.length match {
            case 1 => 
            case 2 => 
                val c1 = Point((knots(0).x * 2 + knot.x) / 3, (knots(0).y * 2 + knot.y) / 3)
                val c2 = Point(2 * c1.x - knots(0).x, (2 * c1.y - knots(0).y))
                c1s += c1
                c2s += c2
            case n =>
                val rhs: ArrayBuffer[(Double, Double)] = ArrayBuffer(
                    Range(1, n-1).map(i => (knots(i).x * 4 + knots(i+1).x * 2, knots(i).y * 4 + knots(i+1).y * 2)):_*).
                    prepend((knots(0).x + knots(1).x * 2, knots(0).y + knots(1).y * 2)).
                    append(((knots(n-2).x * 8 + knots(n-1).x) / 2, (knots(n-2).y * 8 + knots(n-1).y) / 2))
                val (rhsx, rhsy) = rhs.unzip
                val xs = calcC1(rhsx)
                val ys = calcC1(rhsy)
                c1s.clear
                c2s.clear
                Range(0, n-1).foreach({ i => 
                    c1s += Point(xs(i), ys(i))
                    val c2 = 
                        if (i < n-1) Point(knots(i+1).x * 2 - xs(i+1), knots(i+1).y * 2 - ys(i+1))
                        else Point((knots(n-1).x + xs(n-2)) / 2, (knots(n-1).y + ys(n-2)) / 2)
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

    private def solveTridiagonal(a: ArrayBuffer[Double], b: ArrayBuffer[Double], c: ArrayBuffer[Double], rhs: ArrayBuffer[Double]): ArrayBuffer[Double] = {
        val n = rhs.length
        val u = ArrayBuffer.fill(n)(0.0)
        val gam = ArrayBuffer.fill(n)(0.0)
        var bet = b(0)
        u(0) = rhs(0) / bet
        Range(1, n).foreach({ j =>
            gam(j) = c(j-1) / bet
            bet = b(j) - a(j) * gam(j)
            u(j) = (rhs(j) - a(j) * u(j-1)) / bet
        })
        Range(1, n).foreach({ j => 
            u(n-j-1) -= gam(n-j) * u(n-j) 
        })
        u
    }

    private def solveCyclic(a: ArrayBuffer[Double], b: ArrayBuffer[Double], c: ArrayBuffer[Double], alpha: Double, beta: Double, rhs: ArrayBuffer[Double]): ArrayBuffer[Double] = {
        val n = b.length
        val gamma = -b(0)
        val bb = ArrayBuffer.fill(n)(0.0)
        bb(0) = b(0) - gamma
        bb(n-1) = b(n-1) - alpha * beta / gamma
        Range(1, n-1).foreach(i => bb(i) = b(i))

        val solution = solveTridiagonal(a, bb, c, rhs)
        val x = ArrayBuffer.tabulate(n) { case k => solution(k) }
        val u = ArrayBuffer.fill(n)(0.0)
        u(0) = gamma
        u(n-1) = alpha
        val solution_ = solveTridiagonal(a, bb, c, u)
        val z = ArrayBuffer.fill(n)(0.0)
        Range(0, n).foreach(k => z(k) = solution_(k))

        val fact = (x(0) + beta * x(n-1) / gamma) / (1.0 + z(0) + beta * z(n-1) / gamma)
        Range(0, n)foreach(i => x(i) -= fact * z(i))
        x
    }
}
/* 
 * shameless copy of 
 * https://www.codeproject.com/Articles/31859/Draw-a-Smooth-Curve-through-a-Set-of-2D-Points-wit
 * https://www.codeproject.com/Articles/33776/Draw-Closed-Smooth-Curve-with-Bezier-Spline
 */