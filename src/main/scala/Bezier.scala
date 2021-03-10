import scala.annotation.tailrec

object Bezier {

    type Point = (Double, Double)
    type BezierLayer = Seq[Seq[Point]]

    /**
      * Calculate points of Bezier curve
      *
      * @param segmentNo : number of points between start and end points
      * @param ps : points, head & last are start & end points, middle are control points
      * @return : points that constitute Bezier curve, including start & end points
      */
    def apply(segmentNo: Int, ps: Seq[Point]): Seq[Point] = {
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

    // same as apply, including all intermediate results
    def getLayers(segmentNo: Int, ps: Seq[Point]): Seq[BezierLayer] = {
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
        (((p2._1 - p1._1) / total) * n + p1._1, 
         ((p2._2 - p1._2) / total) * n + p1._2)
}