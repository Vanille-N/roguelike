import scala.util.Random
import scala.collection.Iterable
import scala.collection.mutable.Buffer

object Rng extends Random {
    def uniform (min: Int, max: Int): Int = {
        min + this.nextInt(max - min)
    }

    def choice (p: Double): Boolean = {
        p >= (this.nextInt / 100.0)
    }

    def weightedChoice[T] (options: Buffer[Tuple2[Double, T]]): Option[T] = {
        if (options.size == 0) return None
        var tot = 0.0
        options.foreach(tot += _._1)
        // not enough weights, pick uniformly
        if (tot < 0.1) return Some(options(uniform(0, options.size - 1))._2)
        // otherwise choose one
        var curr = 0.0
        var i = -1
        var target = uniform(0, 100) * tot / 100.0
        while (i < options.size - 1) {
            i += 1
            curr += options(i)._1
            if (curr >= target) return Some(options(i)._2)
        }
        Some(options(0)._2) // should not happen except in case of floating point rounding issue
    }

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
