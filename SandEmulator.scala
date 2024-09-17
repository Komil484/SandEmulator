import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

// ' ▄▀█'
object SandEmulator {
  val A = true
  val B = false
  var default_grid = Array(
    Array(B, B, B, B, B, B, B, B, B),
    Array(B, B, B, B, B, B, B, B, B),
    Array(B, A, B, A, B, A, A, A, B),
    Array(B, A, B, A, B, B, A, B, B),
    Array(B, A, A, A, B, B, A, B, B),
    Array(B, A, B, A, B, B, A, B, B),
    Array(B, A, B, A, B, A, A, A, B),
    Array(B, B, B, B, B, B, B, B, B),
    Array(B, B, B, B, B, B, B, B, B)
  )

  def main(args: Array[String]): Unit = {
    val image = if (args.length < 1) {
      get_image_from_grid(default_grid)
    } else {
      val file_path = args(0)
      val file = new File(file_path)
      ImageIO.read(file)
    }

    for (i <- 1 to 100) {
      print_grid(get_grid_from_image(image))
      emulate_image(image)
      Thread.sleep(500)
    }
  }

  def get_grid_from_image(image: BufferedImage): Array[Array[Boolean]] = {
    val white = 0xff_ff_ff_ff

    val width = image.getWidth
    val height = image.getHeight

    var grid = Array.ofDim[Boolean](height, width)

    for (y <- 0 until height; x <- 0 until width) {
      grid(y)(x) = image.getRGB(x, y) != white
    }

    grid
  }

  def get_image_from_grid(grid: Array[Array[Boolean]]): BufferedImage = {
    val white = 0xff_ff_ff_ff
    val black = 0xff_00_00_00

    val width = grid.length
    val height = grid(0).length

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for (y <- 0 until height; x <- 0 until width) {
      image.setRGB(x, y, if (grid(y)(x)) black else white)
    }

    image
  }

  def emulate_image(image: BufferedImage): Unit = {
    def get_emulated_cells(
        topLeft: Int,
        topRight: Int,
        bottomLeft: Int,
        bottomRight: Int
    ): (Int, Int, Int, Int) = {
      val bg_color = 0xff_ff_ff_ff
      var newTL = topLeft
      var newTR = topRight
      var newBL = bottomLeft
      var newBR = bottomRight

      // != bg_color means there is a block
      // == bg_color means it is empty

      // if it is not the case that both tops are empty nor both bots are filled
      if (
        (newTL != bg_color || newTR != bg_color) && (newBL == bg_color || newBR == bg_color)
      ) {
        // if both bots are empty
        if (newBL == bg_color && newBR == bg_color) {
          newBL = newTL
          newTL = bg_color
          newBR = newTR
          newTR = bg_color
        } else {
          // if one is empty
          if (newBL != bg_color) {
            newBR = newTR
            newTR = bg_color
          } else {
            newBL = newTL
            newTL = bg_color
          }
          // if BL and BR are opposite
          // (if one is still empty)
          if ((newBL == bg_color) != (newBR == bg_color)) {
            if (newBL != bg_color) {
              newBR = newTL
              newTL = bg_color
            } else {
              newBL = newTR
              newTR = bg_color
            }
          }
        }
      }
      (newTL, newTR, newBL, newBR)
    }

    val width = image.getWidth
    val height = image.getHeight

    // Process each 2x2 block
    for (y <- height - 2 to 0 by -1; x <- 0 until width - 1) {
      // Extract 2x2 square
      val topLeft = image.getRGB(x, y)
      val topRight = image.getRGB(x + 1, y)
      val bottomLeft = image.getRGB(x, y + 1)
      val bottomRight = image.getRGB(x + 1, y + 1)

      val (newTL, newTR, newBL, newBR) =
        get_emulated_cells(topLeft, topRight, bottomLeft, bottomRight)

      image.setRGB(x, y, newTL)
      image.setRGB(x + 1, y, newTR)
      image.setRGB(x, y + 1, newBL)
      image.setRGB(x + 1, y + 1, newBR)
    }
  }

  def print_grid(grid: Array[Array[Boolean]]): Unit = {
    def printable_from_grid(grid: Array[Array[Boolean]]): List[String] = {
      def processElements(elements: (Boolean, Boolean)): Char = {
        val (elem1, elem2) = elements
        if (elem1) {
          if (elem2) '█'
          else '▀'
        } else {
          if (elem2) '▄'
          else ' '
        }
      }

      val evenGrid: Array[Array[Boolean]] =
        if (grid.length % 2 == 0) grid
        else Array.fill(grid(0).length)(false) +: grid

      evenGrid
        .grouped(2)
        .collect { case Array(col1, col2) =>
          col1.zip(col2).map(processElements).mkString
        }
        .toList
    }

    printable_from_grid(grid).foreach(row => {
      println(row)
    })
  }
}
