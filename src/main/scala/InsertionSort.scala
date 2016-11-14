package idk.yet


abstract class InsertionSort[T <% Ordered[T]] extends BaseSort[T] {

  override def sort(data: Seq[T]): Seq[T] = logger.log("sort")(data) {
    loop( Seq.empty, data )
  }

  def loop(left: Seq[T], right: Seq[T])
          (implicit id: String): Seq[T] =
    if ( right.isEmpty ) left
    else loop( insert( left, right.head ), right.tail )

  def insert(arr: Seq[T], el: T)
            (implicit id: String): Seq[T] = {
    val ( less, greater ) = arr.span( x => x < el )
    (less :+ el ) ++ greater
  }

}


class LogInsertionSort[T <% Ordered[T]]
extends InsertionSort[T]
with VerboseLoggable {

  override def loop(left: Seq[T], right: Seq[T])
                   (implicit id: String): Seq[T] =
    logger.log("loop")(left, right) {
      super.loop(left, right)(id + 1)
    }

  override def insert(arr: Seq[T], el: T)
                     (implicit id: String): Seq[T] =
    logger.log("insert")(arr, el) {
      super.insert(arr, el)(id + 2)
    }

}


final class OptimizedInsertionSort[T <% Ordered[T]]
extends InsertionSort[T]
with OptimizedLoggable {

  override def loop(left: Seq[T], right: Seq[T])
                   (implicit id: String): Seq[T] =
    if ( right.isEmpty ) left
    else loop( insert( left, right.head ), right.tail )

}


object InsertionSort extends SortMaker {

  def makeLog[T <% Ordered[T]] = new LogInsertionSort[T]

  def makeOptimized[T <% Ordered[T]] = new OptimizedInsertionSort[T]

  val name = "InsertionSort"

}
