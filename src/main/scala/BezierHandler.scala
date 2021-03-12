import javafx.event.EventHandler
import javafx.scene.input.MouseEvent
import javafx.scene.{Parent, Group}
import javafx.scene.shape.Circle

case class BezierHandler(p: Group) extends EventHandler[MouseEvent] {
    val knots = collection.mutable.ArrayBuffer.empty[(Double, Double)]

    def handle(e: MouseEvent) = {
        println(s"Handle!! ${e.getX}, ${e.getY}")

        knots += ((e.getX(), e.getY()))
        p.getChildren.add(Knot(e.getX(), e.getY()))
        knots.length match {
            case 1 =>
                
            case _ => 
        }
    }
}

case class Knot(x: Double, y: Double) extends Circle(x, y, 10)
