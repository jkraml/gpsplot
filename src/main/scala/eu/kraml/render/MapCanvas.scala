package eu.kraml.render

import java.awt.geom.Ellipse2D

import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.canvas.Canvas
import eu.kraml.Constants.{TILE_HEIGHT, TILE_WIDTH}
import eu.kraml.io.TileCache
import eu.kraml.model._
import eu.kraml.render.MapCanvas._

import scala.collection.mutable.ArrayBuffer
import scala.math.{ceil, floor}

class MapCanvas(private val tileCache: TileCache, private val boundingBox: BoundingBox, private val zoom: Int) {

    private val (tileOffsetX, tileOffsetY) = boundingBox.northWestCorner.toTileCoord(zoom)
    private val map = initMap
    assembleMap()

    private var glyphs: ArrayBuffer[Glyph] = ArrayBuffer()

    private def initMap:Canvas = {
        val (maxXInTiles, maxYInTiles) = boundingBox.southEastCorner.toTileCoord(zoom)

        val widthInTiles = maxXInTiles - tileOffsetX
        val heightInTiles = maxYInTiles - tileOffsetY

        val widthInPx = (widthInTiles * TILE_WIDTH).toInt
        val heightInPx = (heightInTiles * TILE_HEIGHT).toInt
        Image.filled(widthInPx, heightInPx)
    }

    private def assembleMap(): Unit = {
        val (maxXInTiles, maxYInTiles) = boundingBox.southEastCorner.toTileCoord(zoom)
        val awtG = map.awt.createGraphics //use awt to modify image in place, because it's much faster
        for (x <- floor(tileOffsetX).toInt to ceil(maxXInTiles).toInt;
             y <- floor(tileOffsetY).toInt to ceil(maxYInTiles).toInt) {
            val pixelOffset = toCanvasCoords(x,y)
            val tile = tileCache.get(new TileDescriptor(x,y,zoom))

            awtG.drawImage(tile.awt, pixelOffset.x, pixelOffset.y, null)
        }
        awtG.dispose()
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

    def render: Image = {
        val awtG = map.awt.createGraphics() //use awt to modify image in place, because it's much faster
        //TODO output some kind of progress message
        for (g <- glyphs) {
            g match {
                case Point(center, style) =>
                    style match {
                        case Circle(dia, color) =>
                            val r = dia/2.0
                            val shape = new Ellipse2D.Double(center.x-r, center.y-r, dia, dia)
                            awtG.setColor(color.toAWT)
                            awtG.fill(shape)
                    }
            }
        }
        awtG.dispose()
        map
    }

}

object MapCanvas {
    private case class PxCoord(x: Int, y: Int)

    private sealed trait Glyph
    private case class Point(center: PxCoord, style: PointStyle) extends Glyph

}
