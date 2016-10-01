package eu.kraml.model

import scala.math.{max, min}


//TODO make sure we can put a bounding box over the pacific (it should wrap around the globe)
class BoundingBox {

    private var _east: Double = 0.0
    private var _west: Double = 0.0
    private var _north: Double = 0.0
    private var _south: Double = 0.0

    def this(west: Double, east: Double, south: Double, north: Double) = {
        this()
        _east = east
        _west = west
        _north = north
        _south = south
    }

    def add(point: GpsCoordinate): Unit = {
        _west = min(point.longitude, _west)
        _east = max(point.longitude, _east)
        _south = min(point.latitude, _south)
        _north = max(point.latitude, _north)
    }

    def add(points: Iterable[GpsCoordinate]): Unit = {
        points.foreach( add )
    }

    def contains(point: GpsCoordinate): Boolean = {
        point.latitude >= _south &&
        point.latitude <= _north &&
        point.longitude >= _west &&
        point.longitude <= _east
    }

    def southEastCorner: GpsCoordinate = {
        new GpsCoordinate(_south, _east)
    }

    def southWestCorner: GpsCoordinate = {
        new GpsCoordinate(_south, _west)
    }

    def northEastCorner: GpsCoordinate = {
        new GpsCoordinate(_north, _east)
    }

    def northWestCorner: GpsCoordinate = {
        new GpsCoordinate(_north, _west)
    }

    def width: Double = {
        east-west
    }

    def height: Double = {
        north-south
    }

    def east = _east
    def west = _west
    def south = _south
    def north = _north
}
