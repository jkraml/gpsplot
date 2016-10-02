package eu.kraml.io

import java.io.File
import java.time.Instant
import java.time.format.DateTimeFormatter

import org.scalatest.FlatSpec

//noinspection ZeroIndexToHead
class GpxFileReaderTest extends FlatSpec {
    val minimalXml = new File(getClass.getResource("/minimal.gpx").getFile)

    behavior of "GpxFileReader"

    it should "parse points from minimal.gpx" in {
        val records = GpxFileReader.read(minimalXml).head

        assert(records.size == 3)
    }

    it should "get coordinates from points in minimal.gpx" in {
        val records = GpxFileReader.read(minimalXml).head

        assert(records(0).coordinate.latitude == 1.234567890)
        assert(records(0).coordinate.longitude == 101.234567890)
        assert(records(1).coordinate.latitude == 2.345678901)
        assert(records(1).coordinate.longitude == 12.345678901)
        assert(records(2).coordinate.latitude == 3.456789012)
        assert(records(2).coordinate.longitude == 103.456789012)
    }

    it should "get timestamps from points in minimal.gpx" in {
        val records = GpxFileReader.read(minimalXml).head

        assert(records(0).timestamp equals Instant.parse("2015-01-01T21:21:21Z"))
        assert(records(1).timestamp equals Instant.parse("2015-02-02T22:22:22Z"))
        assert(records(2).timestamp equals Instant.parse("2015-03-03T23:23:23Z"))
    }
}

