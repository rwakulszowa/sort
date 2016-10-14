import scala.util.Random


class Sort(val data: Seq[Int]) {

  def time[T](block: => T): T = {
    val start = System.currentTimeMillis
    val res = block
    val totalTime = System.currentTimeMillis - start
    println( "Elapsed time: %1d ms".format( totalTime ) )
    res
  }

  private def builtinSort(data: Seq[Int]): Seq[Int] = data.sorted

  private def insertionSort(data: Seq[Int]): Seq[Int] = {
    
    def loop(left: Seq[Int], right: Seq[Int]): Seq[Int] = 
      if ( right.isEmpty ) left
      else {
        loop( insert( left, right.head ), right.tail )
      }

    def insert(arr: Seq[Int], el: Int): Seq[Int] = {
      val ( less, greater ) = arr.span( x => x < el )
      (less :+ el ) ++ greater
    }
    
    loop(Seq.empty, data)
  }
  
  def measure = {
    println( "data: " + data )

    println("Builtin sort")
    time{ builtinSort( data )  }

    println("Insertion sort")
    time{ insertionSort( data )  }
  }

}

object Main extends App {
  val problem = new Sort( Seq.fill( 16 )( Random.nextInt ) ) 
  
  problem.measure
}
