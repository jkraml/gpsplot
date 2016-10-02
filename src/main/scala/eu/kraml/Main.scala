package eu.kraml

import java.io.File

import com.sksamuel.scrimage.{Color, Image}
import eu.kraml.Constants.{MAX_ZOOM, MIN_ZOOM}
import eu.kraml.img.MapCanvas
import eu.kraml.img.MapCanvas.Circle
import eu.kraml.io.RenderConfigReader.RenderConfig
import eu.kraml.io.{GpxFileReader, MainConfigReader, RenderConfigReader, TileCache}
import eu.kraml.model.Record

object Main {

    //TODO add logging

    def main(args: Array[String]): Unit = {
        val cmdLineConfig = try {
            parseArgs(args)
        } catch {
            case e:IllegalStateException => sys.exit(1)
        }
        val mainConfig = MainConfigReader.readMainConfig(cmdLineConfig.rootConfig)

        val cache = new TileCache(mainConfig.cacheDir)
        val records: List[Record] = readGpxFiles(mainConfig.dataDir)
        val renderConfigs: Map[String, RenderConfig]= readRenderConfigs(mainConfig.configDir)

        //TODO make sure each render config has a different output file

        renderConfigs foreach {
            case (name, conf) =>
                println("processing " + name)
                val outfile = new File(mainConfig.outputDir, conf.outputFileName)
                render(records, conf, cache).output(outfile)
        }
    }

    private def parseArgs(args: Array[String]): CmdLineConfig = {
        CmdLineConfig.parse(args, CmdLineConfig()) match {
            case Some(parsedConf) => parsedConf
            case None => throw new IllegalStateException("config could not be parsed")
        }
    }

    private def readGpxFiles(dataDir: File): List[Record] = {
        dataDir.listFiles
            .filter(f => f.isFile && f.getName.endsWith(".gpx"))
            .map( GpxFileReader.read)
            .flatMap(_.head)
            .toList
    }

    private def readRenderConfigs(configDir: File): Map[String, RenderConfig] = {
        configDir.listFiles
            .filter(f => f.isFile && f.getName.endsWith(".xml"))
            .flatMap( f =>
                RenderConfigReader.readRenderConfig(f).map(c => f.getName->c)
            )
            .toMap
    }

    private def render(records: Iterable[Record], conf: RenderConfig, cache: TileCache): Image = {
        //TODO iterate over groups
        //TODO filter points in group (date range)

        //TODO check if most recent point or config file is newer than output file
        // - if not, skip rendering (also add override ofr this check)
        val bBox = conf.boundingBox
        val zoom = findZoom(bBox.width, conf.targetWidth)
        println("chose zoom " + zoom)
        val mc = new MapCanvas(cache, bBox, zoom)
        val filteredCoords = records.map(_.coordinate).filter(bBox.contains).toList
        for ((coord,i) <- filteredCoords.zipWithIndex) {
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

        val sortedCandidates = candidates.sortBy {case (zoom, error) => math.pow(error, 2)} // quadratic error

        sortedCandidates.head._1
    }

}
