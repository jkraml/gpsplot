package eu.kraml.render

import java.awt.geom.Ellipse2D
import java.awt.{Color, Graphics2D}

import eu.kraml.Main.ProgressMonitor
import eu.kraml.model.{Circle, PointStyle, Record}

trait RecordRenderer {
    def render(awt: Graphics2D, coordinateConverter: CoordinateConverter, records: List[Record])
              (implicit progress: ProgressMonitor): Unit
}

class CircleRenderer(private val diameter: Int, private val color: Color) extends RecordRenderer {
    private val radius = diameter/2.0

    override def render(awt: Graphics2D, coordinateConverter: CoordinateConverter, records: List[Record])
                       (implicit progress: ProgressMonitor): Unit = {

        awt.setColor(color)

        val process = progress.registerProcess("rendering point")
        process.setMaxValue(records.size)

        records.foreach( r => {
            val center = coordinateConverter.toCanvasCoords(r.coordinate)
            val shape = new Ellipse2D.Double(center.x-radius, center.y-radius, diameter, diameter)
            awt.fill(shape)
            process << 1
        })

        process.indicateCompletion()
    }
}

object RecordRenderer {
    def getRenderer(style: PointStyle): RecordRenderer =
        style match {
            case Circle(d, c) => new CircleRenderer(d, c.toAWT)
            case _ => throw new IllegalArgumentException(s"unknown point style $style")
        }
}