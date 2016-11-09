package idk.yet


object Main extends App {

  val runner = new Benchmark(
    Map(
      "insertion" -> new LogInsertionSort[Int]
    )
  )

  runner.run

}
