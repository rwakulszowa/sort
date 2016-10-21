package idk.yet


object Main extends App {

  val benchmark = new Benchmark(
    Map(
      "insertion" -> new InsertionSort[Int],
      "merge" -> new MergeSort[Int],
      "mergeFlip" -> new MergeFlipSort[Int]
    )
  )

  benchmark.run

}
