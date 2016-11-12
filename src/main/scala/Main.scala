package idk.yet


object Main extends App {

  val runner = new Benchmark(
    Map(
      "bubble" -> BubbleSort.makeOptimized[Int],
      "merge" -> MergeSort.makeOptimized[Int],
      "mergeFlip" -> MergeFlipSort.makeOptimized[Int],
      "insertion" -> InsertionSort.makeOptimized[Int],
      "quick" -> QuickSort.makeOptimized[Int],
      "quickTernary" -> QuickTernarySort.makeOptimized[Int]
    )
  )

  runner.run

}
