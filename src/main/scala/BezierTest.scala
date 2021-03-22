import javafx.application.Application
import javafx.stage.Stage
import javafx.scene.Group
import javafx.scene.Cursor
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.shape.QuadCurve
import javafx.scene.Scene
import javafx.scene.text.Text
import javafx.scene.paint.Color
import javafx.scene.input.{KeyEvent, KeyCode}
import javafx.event.EventHandler
import cats.effect._
import scala.concurrent.duration._
import collection.mutable.ArrayBuffer

object BezierTest {
    def main(as: Array[String]) = {
        Application.launch(classOf[BezierTest_], as:_*)
    }
}

class BezierTest_ extends Application {
    val size = 500
    override def start(ps: Stage) = {
        val root = new Group()
        val can = new Canvas(size, size)
        root.getChildren.add(can)
        val scene = new Scene(root, size, size)
        ps.setScene(scene)
        ps.show

        val bps = BezierSpline(ArrayBuffer.empty[Point], ArrayBuffer.empty[Point], ArrayBuffer.empty[Point], false)
        val gio = FXGIO(can)
        gio.draw(bps)

        val h = FXBezierHandler(bps, gio)
        can.setOnMousePressed(h)
        can.setOnMouseReleased(h)
        can.setOnMouseMoved(h)
        can.setOnMouseDragged(h)

        scene.setOnKeyPressed(new EventHandler[KeyEvent] {
            def handle(e: KeyEvent) = {
                if (bps.closed && e.getCode() == KeyCode.SPACE) {
                    println("Let's fill")
                    // println(bps.bound())
                    gio.fill(bps, Color.CHARTREUSE)
                }
            }
        })
        
        // testBezierApply(can, root, 10)
        // testBezierGetLayers(can, 1)
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
            val bps = Bezier(ps, 15)
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