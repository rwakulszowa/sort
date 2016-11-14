package idk.yet

import scala.util.Random

/** Solves some sorting problems with provided algorithms.
 *
 *  @constructor create a new runner
 *  @param algs a map of algorithm names and classes
 */
class Runner(val algs: List[SortMaker]) {

  def benchmark: List[(String, List[Log])] = {

    val data = Seq.fill( 1000 )( Random.nextInt( 100 ) )

    algs.map(
      algObj => {
        val alg = algObj.makeOptimized[Int]
        alg sort data
        algObj.name -> alg.logger.logs.reverse
      }
    )

  }


  def verbose: List[(String, List[Log])] = {

    val data = Seq.fill( 16 )( Random.nextInt( 16 ) )

    algs.map(
      algObj => {
        val alg = algObj.makeLog[Int]
        alg sort data
        algObj.name -> alg.logger.logs.reverse
      }
    )

  }

  def all = verbose ++ benchmark

}
