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
    def minDistance(start: Point, end: Point, p: Point): Double = 
        minDistance(start, end, p.x, p.y)

    // found at Wikipedia ;)
    def minDistance(p1: Point, p2: Point, x: Double, y: Double): Double = 
        math.abs((p2.x - p1.x)*(p1.y - y) - (p1.x - x) * (p2.y - p1.y)) / 
            math.sqrt(math.pow(p2.x - p1.x, 2) + math.pow(p2.y - p1.y, 2))

    // def on(start: Point, end: Point)(p: Point, tolerance: Double = 1.0) = minDistance(start, end)(p) <= tolerance
}