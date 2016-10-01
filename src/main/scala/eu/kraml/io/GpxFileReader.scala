package eu.kraml.io

import java.io.File
import java.nio.file.Path
import java.time.Instant
import java.time.format.DateTimeFormatter

import eu.kraml.model.{GpsCoordinate, Record}

import scala.collection.mutable.ListBuffer
import scala.xml.XML


object GpxFileReader {
    val formatter = DateTimeFormatter.ISO_INSTANT

    def read(gpxFile: Path): List[Record] = {
        read(gpxFile.toFile)
    }

    def read(gpxFile: File): List[Record] = {
        val gpx = XML.loadFile(gpxFile)
        val segment = gpx \ "trk" \ "trkseg"
        val points = segment \ "trkpt"
        val records = ListBuffer[Record]()
        points.foreach(n => {
            val lat = n.attribute("lat").get.head.text
            val lon = n.attribute("lon").get.head.text
            val timestampText = (n \ "time").text
            val timestamp = Instant.parse(timestampText)
            records += Record(new GpsCoordinate(lat.toDouble, lon.toDouble), timestamp)
        })
        records.toList
    }
}
