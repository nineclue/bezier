import javafx.event.EventHandler
import javafx.scene.input.{MouseEvent, MouseButton}
import javafx.scene.{Parent, Group}
import javafx.scene.shape.{Circle, Line}
import javafx.scene.paint.Color
import javafx.scene.canvas.Canvas
import javafx.scene.Cursor
import collection.mutable.ArrayBuffer

trait PointHandler {
    def setHand(): Unit
    def setGrabbed(): Unit
    def setNormal(): Unit
}

case class FXBezierHandler(bps: BezierSpline, h: GIO) extends EventHandler[MouseEvent] with BezierHandler {
    def handle(e: MouseEvent) = {
        val p = Point(e.getX(), e.getY())
        e.getEventType match {
            case MouseEvent.MOUSE_MOVED =>
                mouseMove(p)
            case MouseEvent.MOUSE_PRESSED =>
                val btn = e.getButton() match {
                    case MouseButton.PRIMARY => 1
                    case MouseButton.SECONDARY => 2
                    case MouseButton.MIDDLE => 3
                    case _ => 0
                }
                mousePressed(p, btn)
            case MouseEvent.MOUSE_RELEASED =>
                mouseReleased(p)
            case MouseEvent.MOUSE_DRAGGED =>
                mouseDragged(p)
            case _ =>
        }

    }
}

case class FXGIO(can: Canvas) extends GIO {
    type C = Color

    val knotFill: C = Color.ORANGE
    val knotStroke: C = Color.SLATEGRAY
    val knotRadius = 10
    val controlFill: C = Color.PAPAYAWHIP
    val controlStroke: C = Color.SLATEGRAY
    val controlRadius = 5
    val lineColor: C = Color.BLACK
    val lineWidth: Double = 3.0
    val cLineColor: C = Color.TOMATO
    val cLineWidth: Double = 1.0
    
    val gc = can.getGraphicsContext2D
    def setHand() = can.setCursor(Cursor.HAND)
    def setGrabbed() = can.setCursor(Cursor.CLOSED_HAND)
    def setNormal() = can.setCursor(Cursor.DEFAULT)
    def drawLine(p1: Point, p2: Point, stroke: C, width: Double): Unit = {
        gc.setLineWidth(width)
        gc.setStroke(stroke)
        gc.strokeLine(p1.x, p1.y, p2.x, p2.y)
    }
    def clear() = gc.clearRect(0, 0, can.getWidth, can.getHeight)

    def drawCircle(p: Point, r: Double, fill: C, stroke: C, strokeWidth: Double): Unit = {
        gc.setLineWidth(strokeWidth)
        gc.setFill(fill)
        gc.setStroke(stroke)
        val x = p.x - r
        val y = p.y - r
        gc.fillOval(x, y, r*2, r*2)
        gc.strokeOval(x, y, r*2, r*2)    
    }
}

