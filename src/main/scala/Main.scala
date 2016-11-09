package idk.yet


object Main extends App {

  val runner = new Benchmark(
    Map(
      "bubble" -> new LogBubbleSort[Int],
      "insertion" -> new LogInsertionSort[Int]
    )
  )

  runner.run

}
