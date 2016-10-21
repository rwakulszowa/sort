package idk.yet

import scala.annotation.tailrec


class MergeSort[T <% Ordered[T]] extends BaseSort[T] {

  override def sort(data: Seq[T]): Seq[T] =
    loop( data )

  private def loop(seq: Seq[T]): Seq[T] =
    if ( seq.length <= 1 ) seq
    else {
      val (left, right) = split( seq )
      merge( Seq.empty )( loop( left ), loop ( right ) )
    }

  private def split(seq: Seq[T]): ( Seq[T], Seq[T] ) =
    seq splitAt seq.length / 2
  
  @tailrec
  private def merge(acc: Seq[T])
                   (left: Seq[T], right: Seq[T]): Seq[T] =
    (left, right) match {
      case (Nil, rs) => acc ++: rs
      case (ls, Nil) => acc ++: ls
      case (l :: ls, r :: rs) => if ( l < r ) merge( acc :+ l )( ls, r :: rs )
                                 else         merge( acc :+ r )( l :: ls, rs )
    }

}
