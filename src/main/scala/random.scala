import scala.util.Random
import scala.collection.Iterable
import scala.collection.mutable.Buffer

/* Wrapper around util.Random
 * - uniform random integers
 * - weighted probabilistic yes/no choice
 * - weighted choice from a probability distribution
 * - geometric law
 */

object Rng extends Random {
    def uniform (min: Int, max: Int): Int = {
        min + this.nextInt(max - min)
    }

    // returns true with probability p
    def choice (p: Double): Boolean = {
        this.nextFloat < p
    }

    def gaussian (base: Int, variability: Int): Int = {
        base + (variability * this.nextGaussian).round.toInt
    }

    type Distribution[T] = Buffer[Tuple2[Double, T]] // probability distribution over T (typically T=Item or T=Direction)
    def null_flatten[T] (item: Option[T]) : Option[T] = {
        item match {
            case None => None
            case Some(null) => None
            case Some(x) => Some(x)
        }
    }
    // choose from probability distribution
    def weightedChoice[T] (options: Distribution[T]): Option[T] = {
        if (options.size == 0) return None
        var tot = 0.0
        options.foreach(tot += _._1)
        // not enough weights, pick uniformly
        if (tot < 0.1) return null_flatten(Some(options(uniform(0, options.size - 1))._2))
        // otherwise choose one
        var curr = 0.0
        var i = -1
        var target = uniform(0, 100) * tot / 100.0
        while (i < options.size - 1) {
            i += 1
            curr += options(i)._1
            if (curr >= target) return null_flatten(Some(options(i)._2))
        }
        null_flatten(Some(options(0)._2)) // should not happen except in case of floating point rounding issue
    }

    // geometric random choice with parameter p
    def priorityChoice[T] (options: Iterable[T], p: Double): Option[T] = {
        var buf: Distribution[T] = Buffer()
        var q = 1.0
        options.iterator.foreach(x => {
            buf.append(Tuple2(q, x))
            q *= 1.0 - p
        })
        weightedChoice(buf)
    }
}
