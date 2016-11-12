package idk.yet

import scala.util.Random

/** Solves some sorting problems with provided algorithms.
 *
 *  @constructor create a new benchmark for some algorithms
 *  @param algs a map of algorithm names and classes
 */
class Benchmark(val algs: List[SortMaker]) {

  val data = Seq.fill( 1000 )( Random.nextInt( 100 ) )

  def time[T](block: => T): (T, Long) = {
    val start = System.currentTimeMillis
    val res = block
    val totalTime = System.currentTimeMillis - start
    ( res, totalTime )
  }

  def run =
    algs.map(a => (a.name, a.makeOptimized[Int])).foreach {
      case( name, alg ) => println(
        name + 
        " - " +
        time { alg sort data }._2 +
        "ms"
      )
    }
}
