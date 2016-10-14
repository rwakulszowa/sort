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

  private def mergeSort(data: Seq[Int]): List[Int] = {

    def split(list: List[Int]): ( List[Int], List[Int] ) =
      list splitAt list.length / 2

    def merge(acc: List[Int])(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
      case (Nil, rs) => acc ++: rs
      case (ls, Nil) => acc ++: ls
      case (l :: ls, r :: rs) => if ( l < r ) merge( acc :+ l )( ls, rs ) 
                                 else merge( acc :+ r )( ls, rs )
    }

    def loop(list: List[Int]): List[Int] =
      if ( list.length <= 1 ) list
      else {
        val (left, right) = split( list )
        merge( Nil )( loop( left ), loop ( right ) )
      }

    loop( data.toList )
  }
 
  def measure = {
    println("Builtin sort")
    time{ builtinSort( data ) }

    println("Insertion sort")
    time{ insertionSort( data ) }

    println("Merge sort")
    time{ mergeSort( data ) }
  }

}

object Main extends App {
  val problem = new Sort( Seq.fill( 1000 )( Random.nextInt( 100 ) ) ) 
  problem.measure
}
