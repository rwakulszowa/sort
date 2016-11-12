package idk.yet

import scala.util.Random

/** Solves some sorting problems with provided algorithms.
 *
 *  @constructor create a new runner
 *  @param algs a map of algorithm names and classes
 */
class Runner(val algs: List[SortMaker]) {

  def benchmark = {

    val data = Seq.fill( 1000 )( Random.nextInt( 100 ) )

    algs.foreach {
      algObj => {
        val alg = algObj.makeOptimized[Int]
        alg.logger.time { alg sort data }
        println(algObj.name)
        println(alg.logger.logs mkString "\n")
        println("")
      }
    }

  }


  def verbose = {

    val data = Seq.fill( 16 )( Random.nextInt( 16 ) )

    algs.foreach {
      algObj => {
        val alg = algObj.makeLog[Int]
        alg sort data
        println(algObj.name)
        println(alg.logger.logs mkString "\n")
        println("")
      }
    }

  }

  def all = {
    verbose
    benchmark
  }

}
