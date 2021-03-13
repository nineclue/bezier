import javafx.event.EventHandler
import javafx.scene.input.MouseEvent
import javafx.scene.{Parent, Group}
import javafx.scene.shape.{Circle, Line}
import javafx.scene.paint.Color
import javafx.beans.property.DoubleProperty
import collection.mutable.ArrayBuffer

trait PointHandler {
    def setHand(): Unit
    def setGrabbed(): Unit
    def setNormal(): Unit
}

case class BezierHandler(bs: ArrayBuffer[Bezier], redraw: () => Unit, ph: PointHandler) extends EventHandler[MouseEvent] {
    var grabbed: Option[(Int, Int)] = None

    def handle(e: MouseEvent) = {
        e.getEventType match {
            case MouseEvent.MOUSE_MOVED =>
                if (grabbed.isEmpty) {
                    val nps = bs.flatMap(_.near(e.getX(), e.getY()))
                    if (nps.nonEmpty) ph.setHand()
                    else ph.setNormal()
                }
            case MouseEvent.MOUSE_PRESSED =>
                grabbed = bs.zipWithIndex.
                    // find adjacent point and pair with Bezier Sequence index
                    flatMap({ case ((b, bi)) => b.near(e.getX(), e.getY()).map(pi => (bi, pi)) }).
                    headOption
                if (grabbed.nonEmpty) ph.setGrabbed()
                else {
                    bs += Bezier.append(bs)
                    redraw()
                }
            case MouseEvent.MOUSE_RELEASED =>
                if (grabbed.nonEmpty) {
                    grabbed = None
                    ph.setNormal()
                }
            case MouseEvent.MOUSE_DRAGGED =>
                grabbed.map({ case (bi, pi) => 
                    bs(bi).update(pi, e.getX(), e.getY())
                    redraw()
                })                
            case _ =>
        }
    }
}