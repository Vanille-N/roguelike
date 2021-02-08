import scala.util.Random

object Rng extends Random {
    def uniform (min: Int, max: Int): Int = {
        min + this.nextInt(max - min)
    }

    def choice (p: Double): Boolean = {
        p >= (this.nextInt / 100)
    }
}
