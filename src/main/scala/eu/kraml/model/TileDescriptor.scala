package eu.kraml.model

import eu.kraml.model.TileDescriptor._

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
            case Top => y
            case Middle => y + 0.5
            case Bottom => y + 1
        }
        val effectiveX = position.horizontalPosition match {
            case Left => x
            case Center => x + 0.5
            case Right => x + 1
        }

        val n = math.pow(2.0, zoom)
        val longitudeInDegrees = effectiveX / n * 360.0 - 180.0
        val latitudeInRadians = atan(sinh(Pi * (1 - 2 * effectiveY / n)))

        new GpsCoordinate(toDegrees(latitudeInRadians), longitudeInDegrees)
    }
}

object TileDescriptor {
    sealed trait VerticalPosition
    case object Top extends VerticalPosition
    case object Middle extends VerticalPosition
    case object Bottom extends VerticalPosition

    sealed trait HorizontalPosition
    case object Left extends HorizontalPosition
    case object Center extends HorizontalPosition
    case object Right extends HorizontalPosition

    sealed abstract class Position(
        val verticalPosition: VerticalPosition,
        val horizontalPosition: HorizontalPosition
    ) {}
    case object TopLeft extends Position(Top, Left)
    case object TopCenter extends Position(Top, Center)
    case object TopRight extends Position(Top, Right)
    case object MiddleLeft extends Position(Middle, Left)
    case object MiddleCenter extends Position(Middle, Center)
    case object MiddleRight extends Position(Middle, Right)
    case object BottomLeft extends Position(Bottom, Left)
    case object BottomCenter extends Position(Bottom, Center)
    case object BottomRight extends Position(Bottom, Right)
}
