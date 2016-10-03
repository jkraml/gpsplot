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

private[render] class HeatMapRenderer() extends RecordRenderer {
    override def render(overlay: BufferedImage, coordinateConverter: CoordinateConverter, records: List[Record])
                       (implicit progress: ProgressMonitor): Unit = {
        val process = progress.registerProcess("rendering heatmap pixel")
        process.setMaxValue(overlay.getWidth*overlay.getHeight)

        //TODO use some kind of index structure for records, the iterations are very slow
        val distanceCutoff = 80.0
        val exponent = 3
        for (y <- 0 until overlay.getHeight;
             x <- 0 until overlay.getWidth) {
            val weight = records.map(_.coordinate)
                .map(coordinateConverter.toCanvasCoords)
                .map(c => math.sqrt(math.pow(c.x - x, 2) + math.pow(c.y - y, 2)))
                .filter(_ < distanceCutoff)
                .map {
                    d  => (distanceCutoff-d)/distanceCutoff
                }
                .map (math.pow(_, exponent))
                .sum
            val clipped = math.min(weight, 255).toInt
            val color = new Color(255,0,0, clipped)
            overlay.setRGB(x,y,color.getRGB)
            process << 1
        }

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