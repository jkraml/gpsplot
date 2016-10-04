package eu.kraml.render

import java.awt.Color
import java.awt.image.BufferedImage

import eu.kraml.Main.EventMonitor
import eu.kraml.model.Record
import eu.kraml.render.MapCanvas.PxCoord


private[render] class HeatMapRenderer() extends RecordRenderer {
    override def render(overlay: BufferedImage, coordinateConverter: CoordinateConverter, records: List[Record])
                       (implicit progress: EventMonitor): Unit = {
        val process = progress.startProcess("rendering heatmap pixel")
        process.setMaxProgressValue(overlay.getWidth * overlay.getHeight)

        val pointWeights: Map[PxCoord, Int] = records
            .map(_.coordinate)
            .map(coordinateConverter.toCanvasCoords)
            .groupBy(identity)
            .mapValues(_.size)

        //TODO use some kind of 2D index structure for point weights to replace the prefilter step
        val distanceCutoff = 80.0
        val exponent = 3
        for (y <- 0 until overlay.getHeight;
             x <- 0 until overlay.getWidth) {
            val weight = pointWeights
                .filter({ //prefilter points which have a chance of being withing the distanceCutoff
                    case (coord, count) => coord.x-x < distanceCutoff && coord.y-y < distanceCutoff
                })
                .map( {
                    case (coord, count) => (math.sqrt(math.pow(coord.x - x, 2) + math.pow(coord.y - y, 2)), count)
                })
                .filter( {
                    case (distance, count) => distance < distanceCutoff
                })
                .map {
                    case (distance, count) =>
                        val linearFactor = (distanceCutoff - distance) / distanceCutoff
                        val modifiedFactor = math.pow(linearFactor, exponent)
                        count * modifiedFactor
                }
                .sum
            val clipped = math.min(weight, 255).toInt
            val color = new Color(255, 0, 0, clipped)
            overlay.setRGB(x, y, color.getRGB)
            process << 1
        }

        process.indicateCompletion()
    }
}
