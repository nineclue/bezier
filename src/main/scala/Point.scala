// import javafx.geometry.Point2D

trait Point {
    def x: Double
    def y: Double
    def near(xx: Double, yy: Double, tolerance: Double = 5.0) = 
        math.sqrt(math.pow(xx - x, 2) + math.pow(yy - y, 2)) <= tolerance
    override def toString(): String = s"Point($x,$y)"
}

object Point {
    def apply(xx: Double, yy: Double) = new Point {
        def x = xx
        def y = yy
    }
}