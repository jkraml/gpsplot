package eu.kraml.render.heatmap

import java.awt.Color
import java.awt.image.BufferedImage

import eu.kraml.Main.EventMonitor
import eu.kraml.model.Record
import eu.kraml.render.MapCanvas.PxCoord
import eu.kraml.render.heatmap.TiledRenderer.{LimitedReach, PixelStyle, makeTiles, tileSize}
import eu.kraml.render.{CoordinateConverter, RecordRenderer}
import eu.kraml.util.TwoDimensionalTree._

import scala.math._


class TiledRenderer(private val pixelStyle: PixelStyle) extends RecordRenderer {

    override def render(overlay: BufferedImage, coordinateConverter: CoordinateConverter, records: List[Record])
                       (implicit progress: EventMonitor): Unit = {

        implicit def coordToPair(coord: PxCoord): (Int, Int) = (coord.x, coord.y)
        val points: Map[(Int, Int), List[Record]] = records
            .groupBy( {
                r => {
                    coordinateConverter.toCanvasCoords(r.coordinate)
                }
            })

        val tree = makeTree(points.toList)

        val extractRecords: (Tree[List[Record]], BoundingBox) => Stream[((Int, Int), List[Record])] =
            pixelStyle match {
                case ps: PixelStyle with LimitedReach =>
                    (tree, tile) => {
                        val distanceCutoffAsInt = ceil(ps.distanceCutoff).toInt
                        val relevantRegion = tile.includeBorder(distanceCutoffAsInt)
                        tree.get(relevantRegion)
                    }
                case ps: PixelStyle =>
                    (tree, tile) => {
                        tree.getAll
                    }
            }

        val process = progress.startProcess("rendering heatmap pixel")
        process.setMaxProgressValue(overlay.getWidth * overlay.getHeight)

        val tiles: Seq[BoundingBox] = makeTiles(overlay.getWidth, overlay.getHeight)
        tiles.foreach( tile => {
            val relevantRecords = extractRecords(tree, tile)
            if (relevantRecords.nonEmpty) {
                for (x <- tile.minX to tile.maxX;
                     y <- tile.minY to tile.maxY) {

                    val color = pixelStyle.evaluatePixel(x,y, relevantRecords)
                    overlay.setRGB(x, y, color.getRGB)
                    process << 1
                }
            } else {
                process << tileSize*tileSize
            }
        })

        process.indicateCompletion()
    }
}

object TiledRenderer {
    private val tileSize = 50

    trait PixelStyle {
        def evaluatePixel(x: Int, y: Int, records: Traversable[((Int, Int), Traversable[Record])]): Color
    }

    trait LimitedReach {
        def distanceCutoff: Int
    }

    private implicit class Crossable[X](xs: Seq[X]) {
        def cross[Y](ys: Seq[Y]) = for { x <- xs; y <- ys } yield (x, y)
    }

    private def makeTiles(width: Int, height: Int): Seq[BoundingBox] = {
        //TODO write a better, more understandable, method
        val xTileBounds = disjoinBounds(makePairs(partition(0, width, tileSize)))
        val yTileBounds = disjoinBounds(makePairs(partition(0, height, tileSize)))

        (xTileBounds cross yTileBounds).map {
            case((minX, maxX), (minY, maxY)) => new BoundingBox(minX, maxX, minY, maxY)
        }
    }

    private def makePairs[T](elements: Seq[T]): Seq[(T,T)] = {
        elements zip elements.tail
    }

    private def disjoinBounds(bounds: Seq[(Int, Int)]): Seq[(Int, Int)] = {
        bounds.map( {
            case (start, end) => (start, end-1)
        })
    }

    private def partition(min: Int, max: Int, step:Int): Seq[Int] = {
        var parts = min.to(max, step)
        if (max % step == 0) {
            parts
        } else {
            parts ++ List(max)
        }
    }
}