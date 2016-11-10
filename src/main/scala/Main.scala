package idk.yet


object Main extends App {

  val runner = new Benchmark(
    Map(
      "bubble" -> new LogBubbleSort[Int],
      "merge" -> new LogMergeSort[Int],
      "mergeFlip" -> new LogMergeFlipSort[Int],
      "insertion" -> new LogInsertionSort[Int],
      "quick" -> new LogQuickSort[Int],
      "quickTernary" -> new LogQuickTernarySort[Int]
    )
  )

  runner.run

}
