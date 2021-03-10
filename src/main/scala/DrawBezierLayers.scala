import Bezier.{Point, BezierLayers}

object DrawBezierLayers {
    type DrawLine = (Point, Point, Int) => Unit
    type DrawPoint = (Point, Point, Int) => Unit

    def draw(line: DrawLine, point: DrawPoint)(ls: BezierLayers) = 
        ls.zipWithIndex.foreach({ (layer, i) => 
            println(s"$i: $layer")
            layer.sliding(2, 1).foreach({ ps =>
                if (ps.length == 2) line(ps(0), ps(1), i)
            })
        })
}