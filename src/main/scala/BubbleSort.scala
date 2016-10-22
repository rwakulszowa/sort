package idk.yet

import scala.annotation.tailrec


class BubbleSort[T <% Ordered[T]] extends BaseSort[T] {

  override def sort(data: Seq[T]): Seq[T] =
    loop( data, Seq.empty )
  
  /** Main bubble sort loop
   *
   *  Find the biggest element in data and prepend it to acc.
   *  
   *  Complexity: O(n) * bubble
   *
   *  Note: total complexity is O(n^3) for lists
   */
  @tailrec
  private def loop(data: Seq[T], acc: Seq[T]): Seq[T] =
    if ( data.isEmpty ) acc
    else {
      val bubbled = bubble( data )
      loop( bubbled.init, bubbled.last +: acc )
    }
  
  /** Push the biggest element to the end of seq
   *
   *  Rebuilds seq using the "swap-if-necessary" algorithm.
   *  Max element will be pushed to the end when done.
   *
   *  Complexity: O(n) * append
   *
   *  Note: the main loop will need to access the last element of seq,
   *  which may be O(1) to O(n), depending on the data structure.
   */
  private def bubble(seq: Seq[T]): Seq[T] =
    seq.foldLeft( Seq.empty[T] )( append )

  /** Append el to seq on last or 2nd last position
   *
   *  Appends a new element to a position depending on seq.last
   *  
   *  Complexity: O(1) to O(n), depending on the data structure
   *
   *  Note: it sucks :/
   */
  private def append(seq: Seq[T], el: T): Seq[T] =
    if ( seq.isEmpty ) Seq[T]( el )
    else {
      if ( el > seq.last ) seq :+ el
      else seq.init :+ el :+ seq.last
    }

}
