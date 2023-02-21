sealed trait Direction

case object Towards extends Direction
case object Away extends Direction
case object Left extends Direction
case object Right extends Direction
case object Unknown extends Direction

/**
 * Determines direction based on the lights noted on a vessel, based on these rules:
 * - Red => The Port side is visible, meaning that the Direction the vessel is
heading Left

 * - Green => The Starboard side is visible, meaning that the Direction the vessel is
heading Right
 * - Green & Red=> Both Port & Starboard sides are visible, meaning the vessel is
heading Towards us(order is irrelevant)
 * - White => We're looking at the aft, meaning the vessel is heading Away
 * - Any other combination means that someone messed up the colours and the heading
is Unknown
 */
object Direction {
  def apply(lights: Seq[Light]): Direction = lights.toList match {
    case Red :: Nil => Left
    case Green :: Nil => Right
    case Red :: Green :: Nil | Green :: Red :: Nil => Towards
    case White :: Nil => Away
    case _ => Unknown
  }
}