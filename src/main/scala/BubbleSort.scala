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
  def loop(data: Seq[T], acc: Seq[T])
          (implicit id: String): Seq[T] =
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
  def bubble(seq: Seq[T])
            (implicit id: String): Seq[T] =
    seq.foldLeft( Seq.empty[T] )( append )

  /** Append el to seq on last or 2nd last position
   *
   *  Appends a new element to a position depending on seq.last
   *  
   *  Complexity: O(1) to O(n), depending on the data structure
   *
   *  Note: it sucks :/
   */
  def append(seq: Seq[T], el: T)
            (implicit id: String): Seq[T] =
    if ( seq.isEmpty ) Seq[T]( el )
    else {
      if ( el > seq.last ) seq :+ el
      else seq.init :+ el :+ seq.last
    }

}


class LogBubbleSort[T <% Ordered[T]] extends BubbleSort[T] {

  override def bubble(seq: Seq[T])
                     (implicit id: String): Seq[T] = 
    Logger.log("bubble") {
      super.bubble(seq)(id + 1)
    }

  override def loop(data: Seq[T], acc: Seq[T])
                   (implicit id: String): Seq[T] = 
    Logger.log("loop") {
      super.loop(data, acc)(id + 2)
    }

  override def append(seq: Seq[T], el: T)
                     (implicit id: String): Seq[T] = 
    Logger.log("append") {
      super.append(seq, el)(id + 3)
    }

}


final class OptimizedBubbleSort[T <% Ordered[T]] extends BubbleSort[T] {

  override def loop(data: Seq[T], acc: Seq[T])
                   (implicit id: String): Seq[T] =
    if ( data.isEmpty ) acc
    else {
      val bubbled = bubble( data )
      loop( bubbled.init, bubbled.last +: acc )
    }

}


object BubbleSort extends SortMaker {
    
  def makeLog[T <% Ordered[T]] = new LogBubbleSort[T]

  def makeOptimized[T <% Ordered[T]] = new OptimizedBubbleSort[T]

  val name = "BubbleSort"

}
