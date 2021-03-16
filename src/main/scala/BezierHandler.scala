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

    def fill(b: BezierSpline, c: C): Unit = {
        val segments = Range(0, b.c1s.length).  // knots may be circular (closed), use c1s (or c2s) size
            map(i => Bezier(b, i)).reduce(_ ++ _)   // all bezier line segments
        val (upleft, lowright) = b.bound()
        Iterator.iterate(upleft.y)(_ + 1).takeWhile(_ <= lowright.y).foreach({ y =>
            val filtered = segments.sliding(2, 1).filter(ps => 
                (ps(0).y >= y && ps(1).y <= y) || (ps(0).y <= y && ps(1).y >= y)).toSeq
            println(s"Y : $y, ${filtered.length} segments")
            // if (filtered.length > 0) println(filtered.map(ps => s"${ps(0)} - ${ps(1)}").mkString(", "))
            var cross = 0
            var startX = 0.0
            Iterator.iterate(upleft.x)(_ + 1).takeWhile(_ <= lowright.x).foreach{ x =>
                val distances = filtered.map(ps => Line.minDistance(ps(0), ps(1))(x, y))
                // println(s"($x, $y) : ${distances.toSeq}")
                distances.minOption match {
                    case Some(d) if d < 1.0 =>
                        // println(s"crossing at $x, $y")
                        cross += 1
                        if (cross % 2 == 1) {
                            startX = x
                        } else {
                            drawLine(Point(startX, y), Point(x, y), c, 1.0)
                        }
                    case _ =>
                }
            }
        })
        println(s"total ${segments.length} segments")
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
                bps.close()
                grabbed = None
                h.draw(bps)
            case None => 
                if (!bps.closed) {  // ignore new point in closed bezier
                    bps.append(p)
                    h.draw(bps)
                }
            case _ =>
                h.setGrabbed()
        }
    }

    def mouseReleased(p: Point) = 
        if (grabbed.nonEmpty) {
            grabbed = None
            h.setNormal()
        }

    def mouseDragged(p: Point) = {
        grabbed match {
            case Some((0, i)) =>
                bps.knots.update(i, p)
                if (i == 0 && bps.closed) // move both start & end knots
                    bps.knots.update(bps.knots.length-1, p)
                h.draw(bps)
            case Some((1, i)) =>
                bps.c1s.update(i, p)
                h.draw(bps)
            case Some((2, i)) =>
                bps.c2s.update(i, p)
                h.draw(bps)
            case _ =>
        }
    }
}
