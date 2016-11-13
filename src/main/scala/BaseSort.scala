package idk.yet

import scala.math.Ordered


abstract class BaseSort[T <% Ordered[T]] extends Loggable {

  def sort(data: Seq[T]): Seq[T]

}


trait SortMaker {

  def makeLog[T <% Ordered[T]]: BaseSort[T]

  def makeOptimized[T <% Ordered[T]]: BaseSort[T]

  val name: String

}
