import java.io.File
import java.awt.image.BufferedImage
import java.awt.{Dimension, Graphics}
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.util.concurrent.CountDownLatch
import javax.imageio.ImageIO
import javax.swing.{JFrame, JPanel, Timer}
import javax.swing.WindowConstants
import scala.util.{Try, Success, Failure}

// ' ▄▀█'
object SandEmulator {
  val escape_char = '\u001b'
  val A = true
  val B = false
  val default_interval = 50
  val default_bg_color = 0xff_ff_ff_ff // white
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
    val parsed_args = parse_args(args)

    parsed_args("help") match {
      case Some(_) => {
        print_help()
        sys.exit(0)
      }
      case None => {}
    }

    val image = get_image_from_args(parsed_args)
    val interval = get_interval_from_args(parsed_args)
    val bg_color = get_bg_color_from_args(parsed_args)
    val scale = get_scale_from_args(parsed_args)

    parsed_args("console") match {
      case Some(_) => {
        val action = new ActionListener {
          override def actionPerformed(e: ActionEvent): Unit = {
            emulate_image(image, bg_color)

            clear_screen()
            print_grid(get_grid_from_image(image, bg_color))
          }
        }

        val timer = new Timer(
          interval,
          action
        )

        save_screen()

        timer.start()

        val latch = new CountDownLatch(1)

        Runtime.getRuntime.addShutdownHook(new Thread() {
          override def run(): Unit = {
            timer.stop()
            latch.countDown()
            restore_screen()
            Runtime.getRuntime.halt(0)
          }
        })

        latch.await()
      }
      case None => {
        val frame = new JFrame("Sane Emulator")
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

        val panel = new JPanel() {
          override def paintComponent(g: Graphics): Unit = {
            super.paintComponent(g)
            g.drawImage(image, 0, 0, null)
          }

          override def getPreferredSize: Dimension =
            new Dimension(image.getWidth, image.getHeight)
        }

        frame.getContentPane.add(panel)
        frame.pack()
        frame.setVisible(true)

        val action = new ActionListener {
          override def actionPerformed(e: ActionEvent): Unit = {
            emulate_image(image, bg_color)

            panel.repaint()
          }
        }

        val timer = new Timer(
          interval,
          action
        )

        timer.start()
      }
    }

  }

  def parse_args(args: Array[String]): Map[String, Option[String]] = {
    val argsMap: Map[String, Option[String]] = args
      .sliding(2)
      .collect {
        case Array(key, value) if key.startsWith("-") => key -> Some(value)
      }
      .toMap

    Map[String, Option[String]](
      "help" -> (if (args.contains("--help") || args.contains("-h")) Some("")
                 else None),
      "input" -> argsMap.getOrElse("--input", argsMap.getOrElse("-i", None)),
      "interval" -> argsMap
        .getOrElse("--interval", argsMap.getOrElse("-t", None)),
      "console" -> (if (args.contains("--console") || args.contains("-c"))
                      Some("")
                    else None),
      "bg-color" -> argsMap.getOrElse(
        "--bg-color",
        argsMap.getOrElse("-b", None)
      ),
      "scale" -> argsMap.getOrElse("--scale", argsMap.getOrElse("-s", None))
    )
  }

  def get_grid_from_image(
      image: BufferedImage,
      bg_color: Int
  ): Array[Array[Boolean]] = {
    val width = image.getWidth
    val height = image.getHeight

    val grid = Array.ofDim[Boolean](height, width)

    for (y <- 0 until height; x <- 0 until width) {
      grid(y)(x) = image.getRGB(x, y) != bg_color
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

  def emulate_image(image: BufferedImage, bg_color: Int): Unit = {
    def get_emulated_cells(
        topLeft: Int,
        topRight: Int,
        bottomLeft: Int,
        bottomRight: Int
    ): (Int, Int, Int, Int) = {
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

    val stringGrid = new StringBuilder()

    printable_from_grid(grid).foreach(row => {
      stringGrid.append(row)
      stringGrid.append("\n")
    })

    print(stringGrid.toString())
  }

  def get_image_from_args(args: Map[String, Option[String]]): BufferedImage = {
    args("input") match {
      case Some(file_path) => {
        val file = new File(file_path)
        Try(ImageIO.read(file)) match {
          case Success(file) => file
          case Failure(ex) => {
            Console.err.println(s"Unable to read file (${file_path})")
            sys.exit(1)
          }
        }
      }
      case None => get_image_from_grid(default_grid)
    }
  }

  def get_interval_from_args(args: Map[String, Option[String]]): Int = {
    args("interval") match {
      case Some(strNum) =>
        Try(strNum.toInt) match {
          case Success(num) => num
          case Failure(ex) => {
            Console.err.println(
              s"Time interval (${strNum}) must be a valid integer"
            )
            sys.exit(1)
          }
        }
      case None => default_interval
    }
  }

  def get_bg_color_from_args(args: Map[String, Option[String]]): Int = {
    // disable transparency
    0xff_00_00_00 | (args("bg-color") match {
      case Some(hex) =>
        Try(Integer.parseInt(hex, 16)) match {
          case Success(num) => num
          case Failure(ex) => {
            Console.err.println(
              s"Background color (${hex}) must be a valid hexadecimal"
            )
            sys.exit(1)
          }
        }
      case None => default_bg_color
    })
  }

  def get_scale_from_args(args: Map[String, Option[String]]): Int = {
    args("scale") match {
      case Some(strNum) =>
        Try(strNum.toInt) match {
          case Success(num) =>
            if (num < 1) {
              Console.err.println(
                s"Scale (${strNum}) must be a positive integer"
              )
              sys.exit(1)
            } else num
          case Failure(ex) => {
            Console.err.println(
              s"Scale (${strNum}) must be a valid positive integer"
            )
            sys.exit(1)
          }
        }
      case None => 1
    }
  }

  def print_help(): Unit = {
    val program_name = System.getProperty("sun.java.command")

    val helpString = s"""
    |Usage: ${program_name} [options]

    |Options:
    |  --help, -h                 Show this help message and exit.
    |  --input, -i <filename>     Specify the input file to be processed.
    |  --interval, -t <interval>  Set the interval (in milliseconds) between operations.
    |  --bg-color, -b <hex color> Specify the background color in hexadecimal format (e.g., #FFFFFF).
    |  --scale, -s <scale>        Set the scale factor (e.g., 1 for no scaling).
    |  --console, -c              Run the program in console mode (no graphical interface).
    """.stripMargin

    println(helpString)
  }

  def save_screen(): Unit = {
    val cursor_invisible = s"${escape_char}[?25l";
    val cursor_save = s"${escape_char} 7"
    val save_screen = s"${escape_char}[?1049h";
    print(cursor_invisible)
    print(save_screen)
  }

  def clear_screen(): Unit = {
    val clear_screen = s"${escape_char}[2J"
    val cursor_home = s"${escape_char}[H"
    print(clear_screen)
    print(cursor_home)
  }

  def restore_screen(): Unit = {
    val cursor_visible = s"${escape_char}[?25h"
    val cursor_restore = s"${escape_char} 8"
    val restore_screen = s"${escape_char}[?1049l"
    print(cursor_visible)
    print(restore_screen)
  }
}
