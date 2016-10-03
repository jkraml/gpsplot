package eu.kraml.render

import java.awt.Color
import java.awt.image.BufferedImage

import eu.kraml.Main.ProgressMonitor
import eu.kraml.model.Record


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
