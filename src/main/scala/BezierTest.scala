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

object BezierTest extends IOApp {
    def run(as: List[String]): IO[ExitCode] = {
        val launchFx = IO.delay(Application.launch(classOf[BezierTest_], as:_*))
        launchFx.as(ExitCode.Success)
    }

    val prepareScene: IO[Scene] = {
        val cSize = 2000
        IO {
            val root = new Group()
            val can = new Canvas(cSize, cSize)
            root.getChildren.add(can)
            new Scene(root, cSize, cSize)
        }
    }
}

class BezierTest_ extends Application {
    val size = 2000
    override def start(ps: Stage) = {
        /*
        val root = new Group()
        val can = new Canvas(size, size)
        root.getChildren.add(can)
        val scene = new Scene(root, size, size)
        ps.setScene(scene)
        */
        ps.show

        /*
        val ioJob = IO({
            val l = new Text("Hello, JavaFX & Cats Effect3")
            root.getChildren.add(l)
        })
        // test(can, root, 10)
        test2(can, 10)
        */
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
            // println(ps)
            // println(bps)
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
                Range(0, 6).map(_ => util.Random.nextDouble * size).
                sliding(2, 2).      // 0, 2, 4...
                map(ps => (ps(0), ps(1))).
                toSeq
            val blayers = Bezier.getLayers(30, ps)
            println(s"BLayers : $blayers")
            blayers.foreach({ _.sliding(2, 1).foreach({ case ps =>
                println(s"Points : $ps")
                if (ps.length <= 2) println(ps)
                else gc.strokeLine(ps(0)._1, ps(0)._2, ps(1)._1, ps(1)._2)
                })
            })
        })
    }
}