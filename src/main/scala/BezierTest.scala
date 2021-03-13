import javafx.application.Application
import javafx.stage.Stage
import javafx.scene.Group
import javafx.scene.Cursor
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.shape.QuadCurve
import javafx.scene.Scene
import javafx.scene.text.Text
import javafx.scene.paint.Color
import cats.effect._
import scala.concurrent.duration._

object BezierTest {
    def main(as: Array[String]) = 
        Application.launch(classOf[BezierTest_], as:_*)
}

class BezierTest_ extends Application {
    val size = 800
    override def start(ps: Stage) = {
        val root = new Group()
        val can = new Canvas(size, size)
        root.getChildren.add(can)
        val scene = new Scene(root, size, size)
        ps.setScene(scene)
        ps.show

        val b = Bezier(Point(100, 100), Point(300, 100), Point(500, 500), Point(700, 700))
        val gc = can.getGraphicsContext2D
        val drawer = BezierDrawer.draw(
            drawCircle(gc, 10, Color.ORANGE, Color.SLATEGRAY, 2.0)_, 
            drawCircle(gc, 5, Color.PAPAYAWHIP, Color.SLATEGRAY, 1.0)_, 
            drawBezierLine(gc)_, drawBezierControlLine(gc)_)
        drawer(b, true)

        val mouseCursorHandler = new PointHandler {
            def setHand() = can.setCursor(Cursor.HAND)
            def setGrabbed() = can.setCursor(Cursor.CLOSED_HAND)
            def setNormal() = can.setCursor(Cursor.DEFAULT)
        }
        val h = BezierHandler(Seq(b), () => { gc.clearRect(0, 0, size, size); drawer(b, true) }, mouseCursorHandler)
        can.setOnMousePressed(h)
        can.setOnMouseReleased(h)
        can.setOnMouseMoved(h)
        can.setOnMouseDragged(h)
        // testBezierApply(can, root, 10)
        // testBezierGetLayers(can, 1)

    }

    def drawBezierLine(gc: GraphicsContext)(p1: Point, p2: Point) = {
        gc.setLineWidth(3.0)
        gc.setStroke(Color.BLACK)
        gc.strokeLine(p1.x, p1.y, p2.x, p2.y)
    }

    def drawBezierControlLine(gc: GraphicsContext)(p1: Point, p2: Point) = {
        gc.setLineWidth(1.0)
        gc.setStroke(Color.TOMATO)
        gc.strokeLine(p1.x, p1.y, p2.x, p2.y)
    }

    def drawCircle(gc: GraphicsContext, r: Double, fill: Color, stroke: Color, strokeWidth: Double)(p: Point) = {
        gc.setLineWidth(strokeWidth)
        gc.setFill(fill)
        gc.setStroke(stroke)
        val x = p.x - r
        val y = p.y - r
        gc.fillOval(x, y, r*2, r*2)
        gc.strokeOval(x, y, r*2, r*2)
    }

    def testBezierApply(can: Canvas, g: Group, n: Int = 5) = {
        val gc = can.getGraphicsContext2D
        val add = 5 // difference between QuadCurve shape & our Bezier curve to ease the check
        Range(0, n).foreach({ _ => 
            gc.setStroke(Color.hsb(util.Random.nextInt(360), 1, 1))
            val ps: Seq[Point] = 
                Range(0, 6).map(_ => util.Random.nextDouble * size).
                sliding(2, 2).      // 0, 2, 4
                map(ps => Point(ps(0), ps(1))).
                toSeq
            val curve = new QuadCurve(ps(0).x, ps(0).y + add, ps(1).x, ps(1).y + add, ps(2).x, ps(2).y + add)
            curve.setOpacity(0.2)
            g.getChildren.add(curve)
            val bps = Bezier(ps)
            bps.sliding(2, 1).foreach({ case ps =>
                gc.strokeLine(ps(0).x, ps(0).y, ps(1).x, ps(1).y)
            })
        })
    }

    def testBezierGetLayers(can: Canvas, n: Int = 5) = {
        val gc = can.getGraphicsContext2D
        Range(0, n).foreach({ _ =>
            // gc.setStroke(Color.hsb(util.Random.nextInt(360), 1, 1))
            val ps: Seq[Point] = 
                Range(0, 8).map(_ => util.Random.nextDouble * size).
                sliding(2, 2).      // 0, 2, 4...
                map(ps => Point(ps(0), ps(1))).
                toSeq
            val blayers = Bezier.getLayers(ps)
            // println(s"BLayers : $blayers")
            DrawBezierLayers.draw(
                { case (p1, p2, i) =>
                    gc.setStroke(Color.grayRgb(0, 1.0 / 4 * i))
                    // gc.setStroke(Color.grayRgb(255, 1.0))
                    gc.strokeLine(p1.x, p1.y, p2.x, p2.y) },
                { case (p1, p2, i) => ()}
            )(blayers)
        })
    }
}