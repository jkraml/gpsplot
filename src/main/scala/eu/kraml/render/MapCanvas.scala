package eu.kraml.render

import java.awt.geom.Ellipse2D

import com.sksamuel.scrimage.Image
import eu.kraml.Constants.{TILE_HEIGHT, TILE_WIDTH}
import eu.kraml.Main.ProgressMonitor
import eu.kraml.io.TileCache
import eu.kraml.model._
import eu.kraml.render.MapCanvas._

import scala.collection.mutable.ArrayBuffer
import scala.math.{ceil, floor}

class MapCanvas(private val tileCache: TileCache, private val boundingBox: BoundingBox, private val zoom: Int) {

    private val (tileOffsetX, tileOffsetY) = boundingBox.northWestCorner.toTileCoord(zoom)


    private var glyphs: ArrayBuffer[Glyph] = ArrayBuffer()

    private def initMap()(implicit progress: ProgressMonitor): Image = {
        val (maxXInTiles, maxYInTiles) = boundingBox.southEastCorner.toTileCoord(zoom)

        val widthInTiles = maxXInTiles - tileOffsetX
        val heightInTiles = maxYInTiles - tileOffsetY

        val widthInPx = (widthInTiles * TILE_WIDTH).toInt
        val heightInPx = (heightInTiles * TILE_HEIGHT).toInt
        val map = Image.filled(widthInPx, heightInPx)

        val drawingProcess = progress.registerProcess("drawing map tiles")

        val minX = floor(tileOffsetX).toInt
        val maxX = ceil(maxXInTiles).toInt
        val minY = floor(tileOffsetY).toInt
        val maxY = ceil(maxYInTiles).toInt
        drawingProcess.setMaxValue((maxX-minX+1)*(maxY-minY+1))

        val awtG = map.awt.createGraphics //use awt to modify image in place, because it's much faster
        for (x <- minX to maxX;
             y <- minY to maxY) {
            val pixelOffset = toCanvasCoords(x,y)
            val tile = tileCache.get(new TileDescriptor(x,y,zoom))

            awtG.drawImage(tile.awt, pixelOffset.x, pixelOffset.y, null)

            drawingProcess << 1
        }
        awtG.dispose()
        drawingProcess.indicateCompletion()

        map
    }

    def addPoint(coordinate: GpsCoordinate, style: PointStyle): Unit = {
        glyphs += Point(toCanvasCoords(coordinate), style)
    }

    private def toCanvasCoords(coordinate: GpsCoordinate): PxCoord = {
        val (tileX,tileY) = coordinate.toTileCoord(zoom)
        toCanvasCoords(tileX, tileY)
    }

    private def toCanvasCoords(tileX: Double, tileY: Double): PxCoord = {
        val pxX = ((tileX-tileOffsetX)*TILE_WIDTH).toInt
        val pxY = ((tileY-tileOffsetY)*TILE_HEIGHT).toInt
        PxCoord(pxX, pxY)
    }

    def render()(implicit progress: ProgressMonitor): Image = {
        val map = initMap()

        val drawingProcess = progress.registerProcess("rendering points")
        drawingProcess.setMaxValue(glyphs.size)
        val awtG = map.awt.createGraphics() //use awt to modify image in place, because it's much faster
        for (g <- glyphs) {
            g match {
                case Point(center, style) =>
                    style match {
                        case Circle(dia, color) =>
                            val r = dia/2.0
                            val shape = new Ellipse2D.Double(center.x-r, center.y-r, dia, dia)
                            awtG.setColor(color.toAWT)
                            awtG.fill(shape)
                            drawingProcess << 1
                    }
            }
        }
        awtG.dispose()
        drawingProcess.indicateCompletion()
        map
    }

}

object MapCanvas {
    private case class PxCoord(x: Int, y: Int)

    private sealed trait Glyph
    private case class Point(center: PxCoord, style: PointStyle) extends Glyph

}
