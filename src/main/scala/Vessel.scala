/**
 * Each vessel has a direction, which is calculated based on it's lightMarks
 * The direction should be calculated based on the fully understood Lights. Meaning,
 * if one of the lightMarks is None
 * we cannot determine the direction. If all lights are known, we should be able to
 * identify it's direction.
 *
 * @param lightMarks
 */
case class Vessel(lightMarks: Seq[String]) {
  def direction: Direction = {
    val lights = lightMarks.map(Light.apply)

    if (lights.contains(None)) Unknown
    else Direction(lights.flatten)
  }
}
