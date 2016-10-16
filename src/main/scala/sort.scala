package idk.yet


/** Exposes some sorting algorithms
 *
 *  Basically, this is just a temporary solution. I'm gonna organize it a bit
 *  better, but not today...
 */
object Sorters {

  def builtinSort(data: Seq[Int]): Seq[Int] = data.sorted

  def insertionSort(data: Seq[Int]): Seq[Int] = {

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

  def mergeSort(data: Seq[Int]): List[Int] = {

    def split(list: List[Int]): ( List[Int], List[Int] ) =
      list splitAt list.length / 2

    def merge(acc: List[Int])(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
      case (Nil, rs) => acc ++: rs
      case (ls, Nil) => acc ++: ls
      case (l :: ls, r :: rs) => if ( l < r ) merge( acc :+ l )( ls, r :: rs )
                                 else merge( acc :+ r )( l :: ls, rs )
    }

    def loop(list: => List[Int]): List[Int] =
      if ( list.length <= 1 ) list
      else {
        val (left, right) = split( list )
        merge( Nil )( loop( left ), loop ( right ) )
      }

    loop( data.toList )
  }

  def mergeFlippingSort(data: Seq[Int]): List[Int] = {
    type Comparator = (Int, Int) => Boolean
    
    val comps: Stream[Comparator] = 
      Stream.continually( List( Ordering[Int].gt _, Ordering[Int].lt _ ).toStream ).flatten

    def split(list: List[Int]): ( List[Int], List[Int] ) =
      list splitAt list.length / 2

    def merge(acc: List[Int])(left: List[Int], right: List[Int])(comp: Comparator): List[Int] = (left, right) match {
      case (Nil, rs) => rs.reverse ++ acc 
      case (ls, Nil) => ls.reverse ++ acc
      case (l :: ls, r :: rs) => if ( comp( l, r ) ) merge( l +: acc )( ls, r :: rs )( comp )
                                 else merge( r +: acc )( l :: ls, rs )( comp )
    }
    
    def loop(list: => List[Int], comps: Stream[Comparator]): List[Int] =
      if ( list.length <= 1 ) list
      else {
        val (left, right) = split( list )
        merge( Nil )( loop( left, comps.tail ), loop( right, comps.tail ) )( comps.head )
      }

    loop( data.toList, comps )
  }

}
