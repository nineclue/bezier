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
    val debugColor: C
    def setHand(): Unit
    def setGrabbed(): Unit
    def setNormal(): Unit
    def drawLine(p1: Point, p2: Point, stroke: C, width: Double): Unit
    def drawCircle(p: Point, r: Double, fill: C, stroke: C, strokeWidth: Double): Unit
    def clear(): Unit
    // FX does not allow get pixel information unless it is drawn & snapshotted
    // def blankCanvas(): GIOCanvas

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
        val segments = b.segments().sliding(2, 1).toSeq
        val (upLeft, lowRight) = b.bound()
        Range(upLeft.y.round.toInt, lowRight.y.round.toInt).foreach({ y => 
            val filtered = segments.filter({ case ps => 
                val ans = (ps(0).y >= y && ps(1).y <= y) || (ps(0).y <= y && ps(1).y >= y)
                // println(s"Y : $y => (${ps(0).x}, ${ps(0).y}) - (${ps(1).x}, ${ps(1).y}), $ans")
                ans
            })
            /*
            Range(upLeft.x.round.toInt, lowRight.x.round.toInt).
                foldLeft((0, 0, -1, Seq.empty[(Int, Int)]))({ case (crossed, contCount, lastX, x =>
            */
            var crossed = 0
            var contCount = 0
            var lastX = -1
            // println(s"Y: $y ${filtered.length}")
            Range(upLeft.x.round.toInt, lowRight.x.round.toInt).foreach({ x =>
                filtered.map({ case ps => Line.minDistance(ps(0), ps(1), x, y)}).minOption match {
                    case Some(d) =>
                        // println(s"($x, $y) => $d")
                        if (d <= 1.0) {
                            if (contCount <= 0) {
                                crossed += 1
                                contCount = 1
                                if (crossed % 2 == 1) { // within boundary
                                    lastX = x
                                } else { // gone out of boundary
                                    // println(s"Draw line!! : ($lastX, $y) - (${x-1}, $y)")
                                    drawLine(Point(lastX, y), Point(x-1, y), c, 1)
                                    lastX = -1
                                }
                            } else {    // still crossing line
                                contCount += 1
                            }
                        } else {
                            contCount = 0
                        } 
                    case None =>
                }
            })
            if (crossed > 1 && crossed % 2 == 1) { 
                println(s"CHECK! $y ($crossed) $lastX $contCount")
                drawLine(Point(0, y), Point(10, y), debugColor, 1.0)
            }
            // if (lastX > 0) println(s"CHECK2!! $y")
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
