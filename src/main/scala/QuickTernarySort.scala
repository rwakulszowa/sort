package idk.yet

import scala.collection.mutable.MutableList


abstract class QuickTernarySort[T <% Ordered[T]] extends BaseSort[T] {

  override def sort(data: Seq[T]): Seq[T] = logger.log("sort")(data) {
    loop( data )
  }

  /** Main quick sort loop
   *
   *  Divide seq into 3 groups until subsequences contain <= 1 element.
   *  Groups contain elements lesser, equal and greater than pivot.
   *  If seq has no repetitions, this algorithm is equal to QuickSort.
   *
   *  Complexity: O(log n) * partition
   */
  def loop(seq: Seq[T])
          (implicit id: String): Seq[T] =
    if ( seq.length <= 1 ) seq
    else {
      val parts = partition( seq )
      val (left, mid, right) = (parts(0), parts(1), parts(2))
      loop( left ) ++ mid  ++ loop( right )
    }

  /** Divide seq into three almost-sorted groups
   *
   *  Divides seq into left, mid and right, where all elements of left are
   *  < pivot, all elements of mid are == pivot, and all elements of right
   *  are > pivot.
   *
   *  Complexity: O(n) + pickPivot
   */
  def partition(seq: Seq[T])
               (implicit id: String): Seq[Seq[T]] = {
    val pivot = pickPivot( seq )
    val ans = List.fill(3)(MutableList.empty[T])

    seq.foreach { x=>
      val index = 
        if (x < pivot) 0
        else if (x > pivot) 2
        else 1
      x +=: ans(index)
    }

    ans
    
  }

  /** Pick a pivot value
   *
   * Pick a value by which the seq will be partitioned. Crucial element
   * of the basic comparison quick sort implementation.
   */
  def pickPivot(seq: Seq[T])
               (implicit id: String): T =
    seq.last

}


class LogQuickTernarySort[T <% Ordered[T]]
extends QuickTernarySort[T]
with VerboseLoggable {

  override def loop(seq: Seq[T])
                   (implicit id: String): Seq[T] =
    logger.log("loop")(seq) {
      super.loop(seq)(id + 1)
    }

  override def partition(seq: Seq[T])
                        (implicit id: String): Seq[Seq[T]] =
    logger.log("partition")(seq) {
      super.partition(seq)(id + 2)
    }

  override def pickPivot(seq: Seq[T])
                        (implicit id: String): T =
    logger.log("pickPivot")(seq) {
      super.pickPivot(seq)(id + 3)
    }

}


final class OptimizedQuickTernarySort[T <% Ordered[T]]
extends QuickTernarySort[T]
with OptimizedLoggable {}


object QuickTernarySort extends SortMaker {

  def makeLog[T <% Ordered[T]] = new LogQuickTernarySort[T]

  def makeOptimized[T <% Ordered[T]] = new OptimizedQuickTernarySort[T]

  val name = "QuickTernarySort"

}
