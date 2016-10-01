package eu.kraml.model

import eu.kraml.Constants.{TILE_HEIGHT, TILE_WIDTH}

import scala.math._


case class GpsCoordinate(latitude: Double, longitude: Double) {
    if (longitude < -180 || longitude > 180)
        throw new IllegalArgumentException(s"longitude must be between -180° and +180°, but is $longitude")
    if (latitude < 0 || latitude > 90)
        throw new IllegalArgumentException(s"latitude must be betwween 0° and 90° but is $latitude")

    def toTile(zoom: Int): TileDescriptor = {
        val (x,y) = toTileCoord(zoom)
        new TileDescriptor(x.toInt, y.toInt, zoom)
    }

    def toPositionWithinTile(zoom: Int): (Int, Int) = {
        val (x,y) = toTileCoord(zoom)
        (((x-x.toInt)*TILE_WIDTH).toInt, ((y-y.toInt)*TILE_HEIGHT).toInt)
    }

    def toTileCoord(zoom: Int): (Double, Double) = {
        if (zoom < 0)
            throw new IllegalArgumentException(s"zoom must be at least 0, but is $zoom")

        val latitudeInRadians = toRadians(latitude)
        val n = pow(2.0, zoom)

        val x = (longitude + 180.0) / 360.0 * n
        val y = (1.0 - log(tan(latitudeInRadians) + (1 / cos(latitudeInRadians))) / Pi) / 2.0 * n
        (x,y)
    }

    def unpack(): (Double, Double) = {
        (latitude, longitude)
    }
}

object GpsCoordinate {
    def clipped(latitude: Double, longitude: Double): GpsCoordinate = {
        new GpsCoordinate(
            clip(0, latitude, 90),
            clip(-180, longitude, 180)
        )
    }

    private  def clip(minimum: Double, value: Double, maximum: Double): Double = {
        max(min(value, maximum), minimum)
    }
}