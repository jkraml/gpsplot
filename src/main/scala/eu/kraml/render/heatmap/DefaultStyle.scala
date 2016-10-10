package eu.kraml.render.heatmap

import java.awt.Color

import eu.kraml.model.Record
import eu.kraml.render.heatmap.TiledRenderer.{LimitedReach, PixelStyle}

import scala.math._


object DefaultStyle extends PixelStyle with LimitedReach {
    private val exponent = 2

    override def evaluatePixel(x: Int, y: Int, records: Traversable[((Int, Int), Traversable[Record])]): Color = {
        val weight = records
            .map( {
                case ((c_x,c_y), points) => (math.sqrt(math.pow(c_x - x, 2) + math.pow(c_y - y, 2)), points.size)
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
        val clipped = min(weight, 255).toInt
        new Color(255, 0, 0, clipped)
    }

    override def distanceCutoff: Int = 70
}
