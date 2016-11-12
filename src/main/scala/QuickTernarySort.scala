package idk.yet


class QuickTernarySort[T <% Ordered[T]] extends BaseSort[T] {

  override def sort(data: Seq[T]): Seq[T] =
    loop( data )
  
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
      val (left, mid, right) = partition( seq )
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
               (implicit id: String): ( Seq[T], Seq[T], Seq[T] ) = {
    val pivot = pickPivot( seq )
    ( seq filter ( a => a < pivot ),
      seq filter ( a => a == pivot ),
      seq filter ( a => a > pivot ) )
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


class LogQuickTernarySort[T <% Ordered[T]] extends QuickTernarySort[T] {
  
  override def loop(seq: Seq[T])
                   (implicit id: String): Seq[T] =
    Logger.log("loop") {
      super.loop(seq)(id + 1)
    }

  override def partition(seq: Seq[T])
                        (implicit id: String): ( Seq[T], Seq[T], Seq[T] ) =
    Logger.log("partition") {
      super.partition(seq)(id + 2)
    }

  override def pickPivot(seq: Seq[T])
                        (implicit id: String): T = 
    Logger.log("pickPivot") {
      super.pickPivot(seq)(id + 3)
    }

}


final class OptimizedQuickTernarySort[T <% Ordered[T]] extends QuickTernarySort[T] {}


object QuickTernarySort extends SortMaker {
    
  def makeLog[T <% Ordered[T]] = new LogQuickTernarySort[T]

  def makeOptimized[T <% Ordered[T]] = new OptimizedQuickTernarySort[T]

}
