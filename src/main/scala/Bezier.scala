import scala.annotation.tailrec

object Bezier {
    type BezierLayer = Seq[Seq[Point]]

    /**
      * Calculate points of Bezier curve
      *
      * @param segmentNo : number of points between start and end points
      * @param ps : points, head & last are start & end points, middle are control points
      * @return : points that constitute Bezier curve, including start & end points
      */
    def apply(ps: Seq[Point], segmentNo: Int = 10): Seq[Point] = {
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