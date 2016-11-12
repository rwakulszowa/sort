package idk.yet

import scala.util.Random

/** Solves some sorting problems with provided algorithms.
 *
 *  @constructor create a new runner
 *  @param algs a map of algorithm names and classes
 */
class Runner(val algs: List[SortMaker]) {


  def time[T](block: => T): (T, Long) = {
    val start = System.currentTimeMillis
    val res = block
    val totalTime = System.currentTimeMillis - start
    ( res, totalTime )
  }


  def benchmark = {

    val data = Seq.fill( 1000 )( Random.nextInt( 100 ) )

    algs.map(a => (a.name, a.makeOptimized[Int])).foreach {
      case( name, alg ) => println(
        name +
        " - " +
        time { alg sort data }._2 +
        "ms"
      )
    }

  }


  def verbose = {

    val data = Seq.fill( 16 )( Random.nextInt( 16 ) )

    algs.map(a => (a.name, a.makeLog[Int])).foreach {
      case( name, alg ) => {
        println(name)
        alg sort data
        println("")
      }
    }

  }

  def all = {
    verbose
    benchmark
  }

}
