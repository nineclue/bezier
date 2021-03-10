import javafx.application.Application
import javafx.stage.Stage
import javafx.scene.Group
import javafx.scene.canvas.Canvas
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
    val size = 2000
    override def start(ps: Stage) = {
        val root = new Group()
        val can = new Canvas(size, size)
        root.getChildren.add(can)
        val scene = new Scene(root, size, size)
        ps.setScene(scene)
        ps.show

        // test(can, root, 10)
        test2(can, 1)
    }

    def test(can: Canvas, g: Group, n: Int = 5) = {
        import Bezier.Point
        val gc = can.getGraphicsContext2D
        val add = 5 // difference between QuadCurve shape & our Bezier curve to ease the check
        Range(0, n).foreach({ _ => 
            gc.setStroke(Color.hsb(util.Random.nextInt(360), 1, 1))
            val ps: Seq[Point] = 
                Range(0, 6).map(_ => util.Random.nextDouble * size).
                sliding(2, 2).      // 0, 2, 4
                map(ps => (ps(0), ps(1))).
                toSeq
            val curve = new QuadCurve(ps(0)._1, ps(0)._2 + add, ps(1)._1, ps(1)._2 + add, ps(2)._1, ps(2)._2 + add)
            curve.setOpacity(0.2)
            g.getChildren.add(curve)
            val bps = Bezier(10, ps)
            bps.sliding(2, 1).foreach({ case ps =>
                gc.strokeLine(ps(0)._1, ps(0)._2, ps(1)._1, ps(1)._2)
            })
        })
    }

    def test2(can: Canvas, n: Int = 5) = {
        import Bezier.Point
        val gc = can.getGraphicsContext2D

        Range(0, n).foreach({ _ =>
            gc.setStroke(Color.hsb(util.Random.nextInt(360), 1, 1))
            val ps: Seq[Point] = 
                Range(0, 8).map(_ => util.Random.nextDouble * size).
                sliding(2, 2).      // 0, 2, 4...
                map(ps => (ps(0), ps(1))).
                toSeq
            val blayers = Bezier.getLayers(30, ps)
            println(s"BLayers : $blayers")
            DrawBezierLayers.draw({ case (p1, p2, i) =>
                gc.strokeLine(p1._1, p1._2, p2._1, p2._2)
            }, { case (p1, p2, i) => ()})(blayers)
        })
    }
}