package eu.kraml.render

import java.awt.Color
import java.awt.geom.Ellipse2D
import java.awt.image.BufferedImage

import eu.kraml.Main.ProgressMonitor
import eu.kraml.model.{Circle, HeatMap, PointStyle, Record}

private[render] trait RecordRenderer {
    def render(overlay: BufferedImage, coordinateConverter: CoordinateConverter, records: List[Record])
              (implicit progress: ProgressMonitor): Unit
}

private[render] class CircleRenderer(private val diameter: Int, private val color: Color) extends RecordRenderer {
    private val radius = diameter/2.0

    override def render(overlay: BufferedImage, coordinateConverter: CoordinateConverter, records: List[Record])
                       (implicit progress: ProgressMonitor): Unit = {

        val awtG = overlay.createGraphics()
        awtG.setColor(color)

        val process = progress.registerProcess("rendering point")
        process.setMaxValue(records.size)

        records.foreach( r => {
            val center = coordinateConverter.toCanvasCoords(r.coordinate)
            val shape = new Ellipse2D.Double(center.x-radius, center.y-radius, diameter, diameter)
            awtG.fill(shape)
            process << 1
        })

        awtG.dispose()
        process.indicateCompletion()
    }
}


private[render] object RecordRenderer {
    def getRenderer(style: PointStyle): RecordRenderer =
        style match {
            case Circle(d, c) => new CircleRenderer(d, c.toAWT)
            case HeatMap() => new HeatMapRenderer()
            case _ => throw new IllegalArgumentException(s"unknown point style $style")
        }
}