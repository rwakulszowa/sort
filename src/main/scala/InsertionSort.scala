class InsertionSort[T <% Ordered[T]] extends BaseSort[T] {

  override def sort(data: Seq[T]): Seq[T] =
    loop(Seq.empty, data)

  def loop(left: Seq[T], right: Seq[T]): Seq[T] =
    if ( right.isEmpty ) left
    else loop( insert( left, right.head ), right.tail )

  def insert(arr: Seq[T], el: T): Seq[T] = {
    val ( less, greater ) = arr.span( x => x < el )
    (less :+ el ) ++ greater
  }

}
