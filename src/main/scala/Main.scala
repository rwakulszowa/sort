package idk.yet

import Sorters._


object Main extends App {

  val benchmark = new Benchmark( Map(
    "builtin" -> builtinSort,
    "insertion" -> insertionSort,
    "merge" -> mergeSort,
    "mergeFlip" -> mergeFlippingSort
  ) )
  benchmark.run

}
