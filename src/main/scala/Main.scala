package idk.yet


object Main extends App {

  val runner = new Benchmark(
    List(
      BubbleSort,
      MergeSort,
      MergeFlipSort,
      InsertionSort,
      QuickSort,
      QuickTernarySort
    )
  )

  runner.run

}
