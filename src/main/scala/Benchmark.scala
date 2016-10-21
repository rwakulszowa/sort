package idk.yet

import scala.util.Random

/** Solves some sorting problems with provided algorithms.
 *
 *  @constructor create a new benchmark for some algorithms
 *  @param algs a map of algorithm names and classes
 */
class Benchmark(val algs: Map[String, BaseSort[Int]]) {

  val data = Seq.fill( 10000 )( Random.nextInt( 100 ) )

  def time[T](block: => T): (T, Long) = {
    val start = System.currentTimeMillis
    val res = block
    val totalTime = System.currentTimeMillis - start
    ( res, totalTime )
  }

  def run =
    algs.foreach {
      case( name, alg ) => println(
        name + 
        " - " +
        time { alg sort data }._2 +
        "ms"
      )
    }
}
