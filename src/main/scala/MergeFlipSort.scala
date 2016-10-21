package idk.yet

import scala.annotation.tailrec


class MergeFlipSort[T <% Ordered[T]] extends BaseSort[T] {

  override def sort(data: Seq[T]): Seq[T] =
    loop( data, comps )

  // An alias for a comparator function: < or >
  type Comparator = (T, T) => Boolean
  
  // A stream (lazy sequence) of alternating comparators
  private val comps: Stream[Comparator] =
    Stream.continually(
      List( Ordering[T].gt _, Ordering[T].lt _ ).toStream
    ).flatten

  private def loop(seq: Seq[T], comps: Stream[Comparator]): Seq[T] =
    if ( seq.length <= 1 ) seq
    else {
      val (left, right) = split( seq )
      merge( comps.head )( Seq.empty )( loop( left, comps.tail ), loop ( right, comps.tail ) )
    }

  private def split(seq: Seq[T]): ( Seq[T], Seq[T] ) =
    seq splitAt seq.length / 2
  
  @tailrec
  private def merge(comp: Comparator)
                   (acc: Seq[T])
                   (left: Seq[T], right: Seq[T]): Seq[T] =
    (left, right) match {
      case (Nil, rs) => rs.reverse ++ acc
      case (ls, Nil) => ls.reverse ++ acc
      case (l :: ls, r :: rs) => if ( comp( l, r ) ) merge( comp )( l +: acc )( ls, r :: rs )
                                 else                merge( comp )( r +: acc )( l :: ls, rs )
    }

}
