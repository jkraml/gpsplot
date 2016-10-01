package eu.kraml.model

import eu.kraml.model.TileDescriptor.Position.Position

import scala.math._

case class TileDescriptor(x: Int, y: Int, zoom: Int) {
    if (zoom < 1)
        throw new IllegalArgumentException(s"zoom must be at least 1, but is $zoom")
    if (x < 0)
        throw new IllegalArgumentException(s"x must be at least 0, but is $x")
    if (y < 0)
        throw new IllegalArgumentException(s"y must be at least 0, but is $y")

    def coordinate(position: Position): GpsCoordinate = {
        val effectiveY = position.verticalPosition match {
            case top => y
            case middle => y + 0.5
            case bottom => y + 1
        }
        val effectiveX = position.horizontalPosition match {
            case left => x
            case center => x + 0.5
            case right => x + 1
        }

        val n = math.pow(2.0, zoom)
        val longitudeInDegrees = effectiveX / n * 360.0 - 180.0
        val latitudeInRadians = atan(sinh(Pi * (1 - 2 * effectiveY / n)))

        new GpsCoordinate(toDegrees(latitudeInRadians), longitudeInDegrees)
    }
}

object TileDescriptor {
    object Position {
        sealed trait VerticalPosition
        case object top extends VerticalPosition
        case object middle extends VerticalPosition
        case object bottom extends VerticalPosition

        sealed trait HorizontalPosition
        private case object left extends HorizontalPosition
        private case object center extends HorizontalPosition
        private case object right extends HorizontalPosition

        sealed abstract class Position(
            val verticalPosition: VerticalPosition,
            val horizontalPosition: HorizontalPosition
        ) {}
        case object topLeft extends Position(top, left)
        case object topCenter extends Position(top, center)
        case object topRight extends Position(top, right)
        case object middleLeft extends Position(middle, left)
        case object middleCenter extends Position(middle, center)
        case object middleRight extends Position(middle, right)
        case object bottomLeft extends Position(bottom, left)
        case object bottomCenter extends Position(bottom, center)
        case object bottomRight extends Position(bottom, right)
    }
}
