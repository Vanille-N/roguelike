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

    // Weighted mean of two colors
    def mix (c1: Color, p1: Double, c2: Color, p2: Double, base: Color = black): Color = {
        val r = (c1.getRed * p1 + c2.getRed * p2 + base.getRed).max(0).min(255)
        val g = (c1.getGreen * p1 + c2.getGreen * p2 + base.getGreen).max(0).min(255)
        val b = (c1.getBlue * p1 + c2.getBlue * p2 + base.getBlue).max(0).min(255)
        new Color(r.toInt, g.toInt, b.toInt)
    }

    def setBlueChannel (c: Color, b: Int): Color = {
        new Color(c.getRed, c.getGreen, c.getBlue.max(b))
    }
}
