object BezierDrawer {
    type PointDraw = Point => Unit
    type LineDraw = (Point, Point) => Unit
    
    def draw(kdraw: PointDraw, cdraw: PointDraw, ldraw: LineDraw, cldraw: LineDraw)(b: Bezier, drawControls: Boolean = false) = {
        b.points.sliding(2, 1).foreach({ case ps => ldraw(ps(0), ps(1)) })
        if (drawControls) {
            cldraw(b.start, b.c1)
            cldraw(b.c2, b.end)
            cdraw(b.c1)
            cdraw(b.c2)
        }
        kdraw(b.start)
        kdraw(b.end)
    }
}