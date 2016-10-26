package idk.yet


class QuickSort[T <% Ordered[T]] extends BaseSort[T] {

  override def sort(data: Seq[T]): Seq[T] =
    loop( data )
  
  /** Main quick sort loop
   *
   *  Divide seq into 2 groups until subsequences contain <= 1 element.
   *  Makes use of the fact, that left.last is the pivot value
   *
   *  Complexity: O(log n) * partition
   */
  private def loop(seq: Seq[T]): Seq[T] = logger log {
    if ( seq.length <= 1 ) seq
    else {
      val (left, right) = partition( seq )
      ( loop( left.init ) :+ left.last ) ++ loop( right )
    }
  }
  
  /** Divide seq into two almost-sorted groups
   *
   *  Divides seq into left and right, where all elements of left are 
   *  <= pivot, and all elements of right are > pivot.
   *
   *  Complexity: O(n) + pickPivot
   */
  private def partition(seq: Seq[T]): ( Seq[T], Seq[T] ) = {
    val pivot = pickPivot( seq )
    ( seq filter ( a => a <= pivot ), seq filter ( a => a > pivot ) )
  }

  /** Pick a pivot value
   *
   * Pick a value by which the seq will be partitioned. Crucial element
   * of the basic comparison quick sort implementation.
   */
  private def pickPivot(seq: Seq[T]): T =
    seq.last
  
}
