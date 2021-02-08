import scala.util.Random

object Rng extends Random {
    def uniform (min: Int, max: Int): Int = {
        min + this.nextInt(max - min)
    }

    def choice (p: Double): Boolean = {
        p >= (this.nextInt / 100)
    def priorityChoice[T] (options: Iterable[T], p: Double): Option[T] = {
        var buf: Buffer[Tuple2[Double, T]] = Buffer()
        var q = 1.0
        options.iterator.foreach(x => {
            buf.append(Tuple2(q, x))
            q *= 1.0 - p
        })
        weightedChoice(buf)
    }
}
