package eu.kraml.render

import java.awt.image.BufferedImage

import eu.kraml.Constants.{TILE_HEIGHT, TILE_WIDTH}
import eu.kraml.Main.EventMonitor
import eu.kraml.io.TileCache
import eu.kraml.model._
import eu.kraml.render.MapCanvas._

import scala.collection.mutable.ListBuffer
import scala.math.{ceil, floor}

//TODO remove scrimmage lib

private[render] class MapCanvas(private val tileCache: TileCache, private val boundingBox: BoundingBox, private val zoom: Int) {

    private val (tileOffsetX, tileOffsetY) = boundingBox.northWestCorner.toTileCoord(zoom)
    private val coordinateConverter = new CoordinateConverter(zoom, tileOffsetX, tileOffsetY)

    private val renderers = new ListBuffer[(RecordRenderer, List[Record])]()

    private def initMap()(implicit progress: EventMonitor): BufferedImage = {
        val (maxXInTiles, maxYInTiles) = boundingBox.southEastCorner.toTileCoord(zoom)

        val widthInTiles = maxXInTiles - tileOffsetX
        val heightInTiles = maxYInTiles - tileOffsetY

        val widthInPx = (widthInTiles * TILE_WIDTH).toInt
        val heightInPx = (heightInTiles * TILE_HEIGHT).toInt
        val map = new BufferedImage(widthInPx, heightInPx, BufferedImage.TYPE_4BYTE_ABGR)

        val drawingProcess = progress.startProcess("drawing map tiles")

        val minX = floor(tileOffsetX).toInt
        val maxX = ceil(maxXInTiles).toInt
        val minY = floor(tileOffsetY).toInt
        val maxY = ceil(maxYInTiles).toInt
        drawingProcess.setMaxProgressValue((maxX-minX+1)*(maxY-minY+1))

        val awtG = map.createGraphics //use awt to modify image in place, because it's much faster
        for (x <- minX to maxX;
             y <- minY to maxY) {
            val pixelOffset = coordinateConverter.toCanvasCoords(x,y)
            val tile = tileCache.get(new TileDescriptor(x,y,zoom))

            awtG.drawImage(tile.awt, pixelOffset.x, pixelOffset.y, null)

            drawingProcess << 1
        }
        awtG.dispose()
        drawingProcess.indicateCompletion()

        map
    }

    def addRenderer(renderer: RecordRenderer, records:List[Record]): Unit = {
        renderers.append((renderer, records))
    }

    def render()(implicit progress: EventMonitor): BufferedImage = {
        val map = initMap()

        //TODO add xml describing how to combine renderers (alpha-blend, overlay), make sure the definition order is kept
        renderers.foreach {
            case (renderer, records) =>
                val overlay = new BufferedImage(map.getWidth, map.getHeight, BufferedImage.TYPE_4BYTE_ABGR)
                renderer.render(overlay, coordinateConverter, records)
                val awtG = map.createGraphics()
                awtG.drawImage(overlay, 0, 0, null)
                awtG.dispose()
        }

        map
    }
}

private[render] class CoordinateConverter(private val zoom: Int,
                          private val tileOffsetX: Double,
                          private val tileOffsetY: Double) {

    def toCanvasCoords(coordinate: GpsCoordinate): PxCoord = {
        val (tileX,tileY) = coordinate.toTileCoord(zoom)
        toCanvasCoords(tileX, tileY)
    }

    def toCanvasCoords(tileX: Double, tileY: Double): PxCoord = {
        val pxX = ((tileX-tileOffsetX)*TILE_WIDTH).toInt
        val pxY = ((tileY-tileOffsetY)*TILE_HEIGHT).toInt
        PxCoord(pxX, pxY)
    }

}

private[render] object MapCanvas {
    case class PxCoord(x: Int, y: Int)
}
