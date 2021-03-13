import Bezier.{BezierLayer}

object DrawBezierLayers {
    type DrawLine = (Point, Point, Int) => Unit
    type DrawPoint = (Point, Point, Int) => Unit

    def draw(line: DrawLine, point: DrawPoint)(ls: Seq[BezierLayer]) = 
        ls.zipWithIndex.foreach({ (layer, i) => 
            drawLayer(line, point)(layer)
        })

    def drawLayer(line: DrawLine, point: DrawPoint)(l: BezierLayer) = 
        l.zipWithIndex.foreach({ (ps, i) => 
            ps.sliding(2, 1).foreach({ pss => 
                if (pss.length == 2) line(pss(0), pss(1), i)
            })
        })
}