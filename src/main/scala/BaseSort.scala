import scala.math.Ordered


abstract class BaseSort[T <% Ordered[T]] {

  def sort(data: Seq[T]): Seq[T]
}
