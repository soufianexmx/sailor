sealed trait Light

case object Green extends Light
case object Red extends Light
case object White extends Light

/**
 * Each Vessel has three different lights on board.[Red, Green, White].
 * Apply function parses a lightMark into a Light object [Red, Green, White].
 * lightMarks are first letter(ignoring case) representations of a color, e.g. r =>
Red , R => Red, g => Green ...
 * If the light isn't recognizable, returns None.
 */
object Light {
  def apply(lightMark: String): Option[Light] = lightMark.toLowerCase match {
    case "r" => Some(Red)
    case "g" => Some(Green)
    case "w" => Some(White)
    case _ => None
  }
}
