import collection.mutable.ArrayBuffer

trait GIO {
    type C

    val knotFill: C
    val knotStroke: C
    val knotRadius: Double
    val controlFill: C
    val controlStroke: C
    val controlRadius: Double
    val lineColor: C
    val lineWidth: Double
    val cLineColor: C
    val cLineWidth: Double

    def setHand(): Unit
    def setGrabbed(): Unit
    def setNormal(): Unit
    def drawLine(p1: Point, p2: Point, stroke: C, width: Double): Unit
    def drawCircle(p: Point, r: Double, fill: C, stroke: C, strokeWidth: Double): Unit
    def clear(): Unit

    def draw(ps: BezierSpline): Unit = {
        clear()
        ps.knots.sliding(2, 1).zipWithIndex.foreach({ case (ks, i) => 
            if (ks.length == 1)
                drawCircle(ks(0), knotRadius, knotFill, knotStroke, 1.0)
            else {            
                // draw line
                val bls = Bezier(ps, i)
                bls.sliding(2, 1).foreach({ case lps =>
                    drawLine(lps(0), lps(1), lineColor, lineWidth)
                })

                // draw control line
                drawLine(ks(0), ps.c1s(i), cLineColor, cLineWidth)
                drawLine(ks(1), ps.c2s(i), cLineColor, cLineWidth)

                // draw controls
                drawCircle(ps.c1s(i), controlRadius, controlFill, controlStroke, 1.0)
                drawCircle(ps.c2s(i), controlRadius, controlFill, controlStroke, 1.0)

                // draw knots
                drawCircle(ks(0), knotRadius, knotFill, knotStroke, 1.0)
                drawCircle(ks(1), knotRadius, knotFill, knotStroke, 1.0)
            }
        })
    }
}

trait BezierHandler {
    val bps: BezierSpline
    val h: GIO

    // point type, point number
    var grabbed: Option[(Int, Int)] = None
    
    private def nearPoints(x: Double, y: Double) = 
        for {
            pi <- 0 to 2
            ps = if (pi == 0) bps._1 else if (pi == 1) bps._2 else bps._3
            i <- Range(0, ps.length)
            if (ps(i).near(x, y))
        } yield (pi, i)

    def mouseMove(p: Point) = {
        if (nearPoints(p.x, p.y).nonEmpty) h.setHand()
        else h.setNormal()
    }

    def mousePressed(p: Point, button: Int) = {
        grabbed = nearPoints(p.x, p.y).headOption
        grabbed match {
            case Some(0, 0) if button == 2 =>  // 1st starting knot
                println("close knot!")

            case None => 
                bps.append(p)
                h.draw(bps)
            case _ =>
                h.setGrabbed()
        }
        /*
        if (grabbed.nonEmpty) {
            if (button == 2 && grabbed.get)
            h.setGrabbed()
        } else {
            Bezier.append(bps, p)
            h.draw(bps)
        }
        */
    }

    def mouseReleased(p: Point) = 
        if (grabbed.nonEmpty) {
            grabbed = None
            h.setNormal()
        }

    def mouseDragged(p: Point) = {
        grabbed match {
            case Some((0, i)) =>
                bps._1.update(i, p)
                h.draw(bps)
            case Some((1, i)) =>
                bps._2.update(i, p)
                h.draw(bps)
            case Some((2, i)) =>
                bps._3.update(i, p)
                h.draw(bps)
            case _ =>
        }
    }
}
