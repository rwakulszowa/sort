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

  def loop(seq: Seq[T], comps: Stream[Comparator])
          (implicit id: String): Seq[T] =
    if ( seq.length <= 1 ) seq
    else {
      val (left, right) = split( seq )
      merge( comps.head )( Seq.empty )( loop( left, comps.tail ), loop ( right, comps.tail ) )
    }

  def split(seq: Seq[T])
           (implicit id: String): ( Seq[T], Seq[T] ) =
    seq splitAt seq.length / 2
  
  def merge(comp: Comparator)
           (acc: Seq[T])
           (left: Seq[T], right: Seq[T])
           (implicit id: String): Seq[T] =
    (left, right) match {
      case (Nil, rs) => rs.reverse ++ acc
      case (ls, Nil) => ls.reverse ++ acc
      case (l :: ls, r :: rs) => if ( comp( l, r ) ) merge( comp )( l +: acc )( ls, r :: rs )
                                 else                merge( comp )( r +: acc )( l :: ls, rs )
    }

}


class LogMergeFlipSort[T <% Ordered[T]] extends MergeFlipSort[T] {
  
  override def loop(seq: Seq[T], comps: Stream[Comparator])
                   (implicit id: String): Seq[T] =
    Logger.log("loop") {
      super.loop(seq, comps)(id + 1)
    }

  override def split(seq: Seq[T])
                    (implicit id: String): ( Seq[T], Seq[T] ) = 
    Logger.log("split") {
      super.split(seq)(id + 2)
    }

  override def merge(comp: Comparator)
                    (acc: Seq[T])
                    (left: Seq[T], right: Seq[T])
                    (implicit id: String): Seq[T] =
    Logger.log("merge") {
      super.merge(comp)(acc)(left, right)(id + 3)
    }

}


final class OptimizedMergeFlipSort[T <% Ordered[T]] extends MergeFlipSort[T] {

  override def merge(comp: Comparator)
                    (acc: Seq[T])
                    (left: Seq[T], right: Seq[T])
                    (implicit id: String): Seq[T] =
    (left, right) match {
      case (Nil, rs) => rs.reverse ++ acc
      case (ls, Nil) => ls.reverse ++ acc
      case (l :: ls, r :: rs) => if ( comp( l, r ) ) merge( comp )( l +: acc )( ls, r :: rs )
                                 else                merge( comp )( r +: acc )( l :: ls, rs )
    }


}


object MergeFlipSort extends SortMaker {
    
  def makeLog[T <% Ordered[T]] = new LogMergeFlipSort[T]

  def makeOptimized[T <% Ordered[T]] = new OptimizedMergeFlipSort[T]

}
