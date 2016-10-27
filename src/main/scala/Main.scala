package idk.yet


object Main extends App {

  val benchmark = new Benchmark(
    Map(
      //"bubble" -> new BubbleSort[Int],  // very very slow
      // TODO: add a timeout
      "insertion" -> new InsertionSort[Int],
      "merge" -> new MergeSort[Int],
      "mergeFlip" -> new MergeFlipSort[Int],
      "quick" -> new QuickSort[Int],
      "quickTernary" -> new QuickTernarySort[Int]
    )
  )

  benchmark.run

}
