package idk.yet

import scala.collection.mutable.MutableList


abstract class QuickNestedSort[T <% Ordered[T]] extends BaseSort[T] {

  val depth = 3  // always a 2^n - 1 (1, 3, 7, 15, ...)

  abstract class Pivot {
    def _locate(x: T, lo: Int, hi: Int): Int

    def locate(x: T) = _locate(x, 0, depth * 2)
  }

  case class PivotNode (value: T, left: Pivot, right: Pivot) extends Pivot {
    override def _locate(x: T, lo: Int, hi: Int): Int = {
      val mid = (lo + hi) / 2
      if ( x == this.value ) mid
      else {
        if (x < this.value) this.left._locate(x, lo, mid - 1)
        else                this.right._locate(x, mid + 1, hi)
      }
    }
  }

  case class PivotLeaf(value: T) extends Pivot {
    override def _locate(x: T, lo: Int, hi: Int): Int = {
      val mid = (lo + hi) / 2
      if ( x == this.value ) mid
      else {
        if ( x < this.value ) lo
        else                  hi
      }
    }
  }

  override def sort(data: Seq[T]): Seq[T] = logger.log("sort")(data) {
    loop( data )
  }

  /** Main quick sort loop
   *
   *  Divide seq into n groups until subsequences contain <= 1 element.
   *  The idea is to use a more fancy comparison function to make the
   *  resulting tree more shallow.
   *
   *  Complexity: O(log n) * partition  // TODO
   */
  def loop(seq: Seq[T])
          (implicit id: String): Seq[T] =
    if ( seq.length <= depth ) seq.sorted
    else {
      val parts = partition( seq )  // odd members have unique elements, even ones gotta be sorted
      parts.zipWithIndex.foldLeft(List.empty[T])((acc, el) => el match {
        case(v, i) => 
          if (i % 2 == 0) acc ++ loop(v)
          else            acc ++ v
      })
    }

  /** Divide seq into some(?) almost-sorted groups
   *
   *  Complexity: O(n)(?) + pickPivot  // TODO
   */
  def partition(seq: Seq[T])
               (implicit id: String): Seq[Seq[T]] = {
    val pivot = pickPivot( seq )
    var ans = List.fill(2 * depth + 1)(MutableList.empty[T])  // a list of empty lists

    seq.foreach { x =>
      val index = pivot.locate(x)
      x +=: ans(index)
    }

    ans
  }

  /** Pick a pivot tree
   *
   *  Constructs a binary tree of pivot values
   */
  def pickPivot(seq: Seq[T])
                (implicit id: String): Pivot = {

    def makePivot(els: Seq[T]): Pivot = 
      if (els.length == 1) PivotLeaf(els.head)
      else {
        val midIndex = els.length / 2
        PivotNode( els(midIndex), makePivot(els.take(midIndex)), makePivot(els.takeRight(midIndex)) )
      }
    
    makePivot(seq.take(depth).sorted)
                
  }

}


class LogQuickNestedSort[T <% Ordered[T]]
extends QuickNestedSort[T]
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
                        (implicit id: String): Pivot =
    logger.log("pickPivot")(seq) {
      super.pickPivot(seq)(id + 3)
    }

}


final class OptimizedQuickNestedSort[T <% Ordered[T]]
extends QuickNestedSort[T]
with OptimizedLoggable {}


object QuickNestedSort extends SortMaker {

  def makeLog[T <% Ordered[T]] = new LogQuickNestedSort[T]

  def makeOptimized[T <% Ordered[T]] = new OptimizedQuickNestedSort[T]

  val name = "QuickNestedSort"

}
