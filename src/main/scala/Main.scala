package idk.yet


object Main extends App {

  val runner = new Benchmark(
    Map(
      "bubble" -> BubbleSort.makeLog[Int],
      "merge" -> MergeSort.makeLog[Int],
      "mergeFlip" -> MergeFlipSort.makeLog[Int],
      "insertion" -> InsertionSort.makeLog[Int],
      "quick" -> QuickSort.makeLog[Int],
      "quickTernary" -> QuickTernarySort.makeLog[Int]
    )
  )

  runner.run

}
