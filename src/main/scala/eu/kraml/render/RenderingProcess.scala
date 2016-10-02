package eu.kraml.render

import java.io.File
import java.time.Instant

import eu.kraml.Constants._
import eu.kraml.Main.ProgressMonitor
import eu.kraml.io.TileCache
import eu.kraml.model.{PointStyle, Record, RenderConfig}
import eu.kraml.render.RenderingProcess.{findZoom, mostRecent}

import scala.collection.mutable

class RenderingProcess(val cache: TileCache, val mainConfigModificationDate: Instant, val outputDir: File, var forceRender: Boolean = true) {

    def render(records: Iterable[Record], conf: RenderConfig, configModificationDate: Instant)
              (implicit progress: ProgressMonitor): Unit = {
        val outfile = new File(outputDir, conf.outputFileName)
        val bBox = conf.boundingBox
        val recordsInBBox = records.filter(r => bBox.contains(r.coordinate)).toList
        val recordsAndStyles = new mutable.ListBuffer[(List[Record], PointStyle)]()
        var mostRecentRelevantDate = mostRecent(mainConfigModificationDate, configModificationDate)

        conf.groups.foreach( groupConf => {
            val matchingRecords = recordsInBBox
                .filter(groupConf.filter.recordMatches)
            recordsAndStyles.append((matchingRecords, groupConf.style))

            val newestRecordDate = matchingRecords.map(_.timestamp).sorted.reverse.head
            mostRecentRelevantDate = mostRecent(mostRecentRelevantDate, newestRecordDate)
        })

        val lastRenderDate = Instant.ofEpochSecond(outfile.lastModified()) //also covers the case where output file does not exist
        if (!forceRender && (mostRecentRelevantDate isBefore lastRenderDate)) {
            println("nothing has changed since last rendering")
            return
        }

        val zoom = findZoom(bBox.width, conf.targetWidth)
        val canvas = new MapCanvas(cache, bBox, zoom)

        recordsAndStyles.foreach {
            case (filteredRecords, style) =>
                val renderer = RecordRenderer.getRenderer(style)
                canvas.addRenderer(renderer, filteredRecords)
        }

        canvas.render.output(outfile)
    }
}

object RenderingProcess {

    private def mostRecent(instants: Instant*): Instant = {
        instants.reduce((i1, i2) => {
            if (i1 isAfter i2)
                i1
            else
                i2
        })
    }

    private def findZoom(widthInDegrees: Double, targetWidthInPx: Int):Int = {
        val targetWidthInTiles = targetWidthInPx / TILE_WIDTH
        val widthInPercentOfWorld = widthInDegrees / 360.0

        val candidates: List[(Int, Double)] =
            (MIN_ZOOM to MAX_ZOOM).map(z => {
                val width = math.pow(2,z) * TILE_WIDTH * widthInPercentOfWorld
                val error = width - targetWidthInPx
                (z, error)
            }).toList

        val sortedCandidates = candidates.sortBy {case (zoom, error) => math.pow(error, 2)} // quadratic error

        sortedCandidates.head._1
    }
}
