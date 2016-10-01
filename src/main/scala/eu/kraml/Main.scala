package eu.kraml

import java.io.File

import com.sksamuel.scrimage.{Color, Image}
import eu.kraml.Constants.{MAX_ZOOM, MIN_ZOOM}
import eu.kraml.img.MapCanvas
import eu.kraml.img.MapCanvas.Circle
import eu.kraml.io.RenderConfigReader.RenderConfig
import eu.kraml.io.{GpxFileReader, MainConfigReader, RenderConfigReader, TileCache}
import eu.kraml.model.Record

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Main {

    def main(args: Array[String]): Unit = {
        val mainConfig = MainConfigReader.readMainConfig(args(1))

        val cache = new TileCache(mainConfig.cacheDir)

        val records: ArrayBuffer[Record] = ArrayBuffer()
        for (f <- mainConfig.dataDir.listFiles.filter(f => f.isFile && f.getName.endsWith(".gpx"))) {
            try {
                print("reading GPX file " + f.getPath + " ")
                GpxFileReader.read(f).foreach(records.+=)
                println("done")
            } catch {
                case e:Exception =>
                    println("failed")
            }
        }

        val renderConfigs: mutable.Map[String, RenderConfig]= mutable.HashMap.empty
        for (f <- mainConfig.configDir.listFiles.filter(f => f.isFile && f.getName.endsWith(".xml")))
            try {
                print("reading render config " + f.getPath + " ")
                renderConfigs += (f.getPath -> RenderConfigReader.readRenderConfig(f))
                println("done")
            } catch {
                case e:Exception =>
                    println("failed")
                    e.printStackTrace()
            }

        renderConfigs foreach {
            case (name, conf) =>
                println("processing " + name)
                val outfile = new File(mainConfig.outputDir, conf.outputFileName)
                render(records, conf, cache).output(outfile)
        }

    }

    private def render(records: Iterable[Record], conf: RenderConfig, cache: TileCache): Image = {
        //TODO iterate over groups
        //TODO filter points in group (date range)
        val bBox = conf.boundingBox
        val zoom = findZoom(bBox.width, conf.targetWidth)
        println("chose zoom "+zoom)
        val mc = new MapCanvas(cache, bBox, zoom)
        val filteredCoords = records.map(_.coordinate).filter(bBox.contains).toList
        for ((coord,i) <- filteredCoords.zipWithIndex) {
            println("rendering point "+i)
            mc.addPoint(coord, Circle(10, Color.apply(255,0,0)))
        }
        mc.image
    }

    private def findZoom(widthInDegrees: Double, targetWidthInPx: Int):Int = {
        val targetWidthInTiles = targetWidthInPx / 256.0
        val widthInPercentOfWorld = widthInDegrees/360.0

        val candidates: List[(Int, Double)] =
        (MIN_ZOOM to MAX_ZOOM).map(z => {
            val width = math.pow(2,z)*256.0*widthInPercentOfWorld
            val error = width-targetWidthInPx
            (z,error)
        }).toList

        val sortedCandidates = candidates.sortBy(tuple => math.pow(tuple._2, 2)) // quadratic error

        sortedCandidates.head._1
    }

}
