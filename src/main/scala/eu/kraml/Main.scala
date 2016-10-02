package eu.kraml

import java.io.File
import java.time.Instant

import eu.kraml.Constants.{MAX_ZOOM, MIN_ZOOM, TILE_WIDTH}
import eu.kraml.io.{GpxFileReader, MainConfigReader, RenderConfigReader, TileCache}
import eu.kraml.model.{InvocationConfig, PointStyle, Record, RenderConfig}
import eu.kraml.render.MapCanvas

import scala.collection.mutable

object Main {

    //TODO add logging

    def run(invocationConfig: InvocationConfig): Unit = {
        invocationConfig.assertConfigurationIsComplete()
        val mainConfig = MainConfigReader.readMainConfig(invocationConfig.rootConfig)
        val mainConfigModificationDate = Instant.ofEpochSecond(invocationConfig.rootConfig.lastModified())

        val cache = new TileCache(mainConfig.cacheDir)
        val records: List[Record] = readGpxFiles(mainConfig.dataDir)
        val renderConfigs: Map[String, (RenderConfig, Instant)]= readRenderConfigs(mainConfig.configDir)
        warnIfSameOutputFilesAreUsed(renderConfigs)

        renderConfigs foreach {
            case (name, (conf, modDate)) =>
                println("processing " + name)
                render(records, conf, cache, mainConfig.outputDir, modDate, invocationConfig.forceRender)
        }
    }

    private def warnIfSameOutputFilesAreUsed(renderConfigs: Map[String, (RenderConfig, Instant)]): Unit = {
        val outputFileToConfigs = new mutable.HashMap[String, mutable.Set[String]] with mutable.MultiMap[String, String]
        renderConfigs.foreach {
            case (confFile, (config, modDate)) => outputFileToConfigs.addBinding(config.outputFileName, confFile)
        }

        outputFileToConfigs
            .filter({
                case (outFile, configFiles) => configFiles.size > 1
            })
            .foreach( {
                case (outFile, configFiles) =>
                    val filesString = configFiles.mkString(",")
                    println(s"Config files $filesString all write their output to $outFile" )
            })
    }

    private def readGpxFiles(dataDir: File): List[Record] = {
        dataDir.listFiles
            .filter(f => f.isFile && f.getName.endsWith(".gpx"))
            .map( GpxFileReader.read)
            .flatMap(_.head)
            .toList
    }

    private def readRenderConfigs(configDir: File): Map[String, (RenderConfig, Instant)] = {
        configDir.listFiles
            .filter(f => f.isFile && f.getName.endsWith(".xml"))
            .flatMap( f => {
                val modDate = Instant.ofEpochSecond(f.lastModified())
                    RenderConfigReader.readRenderConfig(f).map(c => f.getName -> (c, modDate))
            })
            .toMap
    }

    //TODO move render method to own class (use one instance for each output file)
    private def render(records: Iterable[Record], conf: RenderConfig, cache: TileCache, outputDir: File, configModificationDate: Instant, forceRender: Boolean = true): Unit = {
        val outfile = new File(outputDir, conf.outputFileName)
        val bBox = conf.boundingBox
        val recordsInBBox = records.filter(r => bBox.contains(r.coordinate)).toList
        val recordsAndStyles = new mutable.ListBuffer[(List[Record], PointStyle)]()
        var mostRecentRelevantDate = configModificationDate

        conf.groups.foreach( groupConf => {
            val matchingRecords = recordsInBBox
                .filter(groupConf.filter.recordMatches)
            recordsAndStyles.append((matchingRecords, groupConf.style))

            val newestRecordDate = matchingRecords.map(_.timestamp).sorted.reverse.head
            mostRecentRelevantDate = mostRecent(mostRecentRelevantDate, newestRecordDate)
        })

        val lastRenderDate = Instant.ofEpochSecond(outfile.lastModified())
        if (!forceRender && (mostRecentRelevantDate isBefore lastRenderDate)) {
            println("nothing has changed since last rendering")
            return
        }

        val zoom = findZoom(bBox.width, conf.targetWidth)
        println("chose zoom " + zoom)
        val canvas = new MapCanvas(cache, bBox, zoom)

        recordsAndStyles.foreach {
            case (filteredRecords, style) =>
                filteredRecords
                    .map(_.coordinate)
                    .foreach(c => canvas.addPoint(c, style))
        }

        canvas.render.output(outfile)
    }

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
