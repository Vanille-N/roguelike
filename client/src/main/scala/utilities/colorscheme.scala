import java.awt.Color

/* Color Scheme: visual palette
 * - redefinitions of common colors
 * - color utilities
 */

object Scheme {
    // Global palette
    val black = new Color(25, 25, 25)
    val white = new Color(220, 220, 220)
    val darkGray = new Color(70, 70, 70)
    val mediumGray = new Color(120, 120, 120)
    val lightGray = new Color(170, 170, 170)
    val red = new Color(200, 10, 10)
    val green = new Color(10, 200, 10)
    val purple = new Color(100, 0, 100)

    // Weighted mean of two colors
    def mix (items: List[Tuple2[Color, Double]]): Color = {
        var r = 0.0
        var g = 0.0
        var b = 0.0
        items.foreach(_ match {
            case (c, p) => {
                r += c.getRed * p
                g += c.getGreen * p
                b += c.getBlue * p
            }
        })
        r = r.max(0).min(255)
        g = g.max(0).min(255)
        b = b.max(0).min(255)
        new Color(r.toInt, g.toInt, b.toInt)
    }

    def setBlueChannel (c: Color, b: Int): Color = {
        new Color(c.getRed, c.getGreen, c.getBlue.max(b))
    }
}
