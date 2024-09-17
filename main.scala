// ' ▄▀█'
object Main {
  def main(args: Array[String]): Unit = {
    val chars = " ▄▀█"
    val grid = List(
      List(true, true, false),
      List(true, false, true),
      List(false, false, true)
    )
    print_grid(grid)
  }

  def print_grid(grid: List[List[Boolean]]): Unit = {
    def printable_from_grid(grid: List[List[Boolean]]): List[String] = {
      def processElements(elem1: Boolean, elem2: Boolean): Char = {
        if (elem1) {
          if (elem2) '█'
          else '▀'
        } else {
          if (elem2) '▄'
          else ' '
        }
      }

      val evenGrid: List[List[Boolean]] =
        if (grid.length % 2 == 0) grid
        else List.fill(grid.head.length)(false) :: grid

      evenGrid
        .grouped(2)
        .map {
          case Seq(col1, col2) => col1.zip(col2).map(processElements).mkString
          case _               => ""
        }
        .toList
    }

    printable_from_grid(grid).foreach(row => {
      println(row)
    })
  }
}
