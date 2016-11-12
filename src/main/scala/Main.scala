package idk.yet


object Main extends App {

  val runner = new Runner(
    List(
      BubbleSort,
      MergeSort,
      MergeFlipSort,
      InsertionSort,
      QuickSort,
      QuickTernarySort
    )
  )

  runner.all

}
