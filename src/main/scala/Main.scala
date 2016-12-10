package idk.yet


object Main extends App {

  val runner = new Runner(
    List(
      BubbleSort,
      MergeSort,
      MergeFlipSort,
      InsertionSort,
      QuickSort,
      QuickTernarySort,
      QuickNestedSort
    )
  )

  val results = runner.all

  results.foreach {
    case (name, logs) => {
      println(name)
      println(logs mkString "\n")
      println()
    }
  }

}
