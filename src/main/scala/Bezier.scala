object Bezier {

    type Point = (Double, Double)

    /**
      * Calculate points of Bezier curve
      *
      * @param segmentNo : number of points between start and end points
      * @param ps : points, head & last are start & end points, middle are control points
      * @return : points that constitute Bezier curve, including start & end points
      */
    def apply(segmentNo: Int, ps: Seq[Point]): Seq[Point] = {
        def h(n: Int, pss: Seq[Point]): Point = 
            if (pss.size == 1) pss(0)
            else h(n, pss.sliding(2, 1).map({ case pa => 
                nth(pa(0), pa(1), n, segmentNo)}).toSeq)
        Range(0, segmentNo).
            map(i => h(i, ps)).toSeq.
            prepended(ps.head).appended(ps.last)
    }

    private def nth(p1: Point, p2: Point, n: Int, total: Int): Point = 
        (((p2._1 - p1._1) / total) * n + p1._1, 
         ((p2._2 - p1._2) / total) * n + p1._2)
}