package idk.yet

import scala.annotation.tailrec


class MergeSort[T <% Ordered[T]] extends BaseSort[T] {

  override def sort(data: Seq[T]): Seq[T] =
    loop( data )

  def loop(seq: Seq[T])
          (implicit id: String): Seq[T] = 
    if ( seq.length <= 1 ) seq
    else {
      val (left, right) = split( seq )
      merge( Seq.empty )( loop( left ), loop ( right ) )
    }

  def split(seq: Seq[T])
           (implicit id: String): ( Seq[T], Seq[T] ) = 
    seq splitAt seq.length / 2
  
  def merge(acc: Seq[T])
           (left: Seq[T], right: Seq[T])
           (implicit id: String): Seq[T] =
    (left, right) match {
      case (Nil, rs) => acc ++: rs
      case (ls, Nil) => acc ++: ls
      case (l :: ls, r :: rs) => if ( l < r ) merge( acc :+ l )( ls, r :: rs )
                                 else         merge( acc :+ r )( l :: ls, rs )
    }

}


class LogMergeSort[T <% Ordered[T]] extends MergeSort[T] {
  
  override def loop(seq: Seq[T])
                   (implicit id: String): Seq[T] =
    Logger.log("loop") {
      super.loop(seq)(id + 1)
    }

  override def split(seq: Seq[T])
                    (implicit id: String): ( Seq[T], Seq[T] ) = 
    Logger.log("split") {
      super.split(seq)(id + 2)
    }

  override def merge(acc: Seq[T])
                    (left: Seq[T], right: Seq[T])
                    (implicit id: String): Seq[T] =
    Logger.log("merge") {
      super.merge(acc)(left, right)(id + 3)
    }

}


final class OptimizedMergeSort[T <% Ordered[T]] extends MergeSort[T] {

  override def merge(acc: Seq[T])
                    (left: Seq[T], right: Seq[T])
                    (implicit id: String): Seq[T] =
    (left, right) match {
      case (Nil, rs) => acc ++: rs
      case (ls, Nil) => acc ++: ls
      case (l :: ls, r :: rs) => if ( l < r ) merge( acc :+ l )( ls, r :: rs )
                                 else         merge( acc :+ r )( l :: ls, rs )
    }

}



object MergeSort {
    
  def makeLog[T <% Ordered[T]] = new LogMergeSort[T]

  def makeOptimized[T <% Ordered[T]] = new OptimizedMergeSort[T]

}
