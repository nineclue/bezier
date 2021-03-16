// import javafx.geometry.Point2D

case class Point(x: Double, y: Double) {
    def distance(p: Point) = Point.distance(p.x, p.y, x, y)
    def distance(xx: Double, yy: Double) = Point.distance(xx, yy, x, y)

    def near(p: Point, tolerance: Double) = distance(p) <= tolerance
    def near(xx: Double, yy: Double, tolerance: Double = 5.0) = distance(xx, yy) <= tolerance
}

object Point {
    def distance(x1: Double, y1: Double, x2: Double, y2: Double) = 
        math.sqrt(math.pow(x1 - x2, 2) + math.pow(y1 - y2, 2))
}

object Line {
    /*
    def minDistance(start: Point, end: Point)(p: Point) = {
        val slope = (end.y - start.y) / (end.x - start.x)
        val (startx, endx) = if (start.x <= end.x) (start.x, end.x) else (end.x, start.x)
        Iterator.iterate(startx)(_ + 1).takeWhile(_ <= endx).toSeq.minBy({ x =>
            val y = (x - startx) * slope
            p.distance(x, y)
        })
    }
    */

    def minDistance(start: Point, end: Point)(x: Double, y: Double) = {
        val slope = (end.y - start.y) / (end.x - start.x)
        val (startx, endx, starty) = if (start.x <= end.x) (start.x, end.x, start.y) else (end.x, start.x, end.y)
        // println(s"$startx, $endx, $slope")
        Iterator.iterate(startx)(_ + 1).takeWhile(_ <= endx).map({ xx =>
            val yy = (xx - startx) * slope + starty
            val d = Point.distance(x, y, xx, yy)
            // println(s"($xx, $yy) - ($x, $y) => $d = ($xx - $startx) * $slope")
            d
        }).min
    }

    // def on(start: Point, end: Point)(p: Point, tolerance: Double = 1.0) = minDistance(start, end)(p) <= tolerance
}