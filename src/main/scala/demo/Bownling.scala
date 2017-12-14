package demo


object Bowling {


  private[this] def process(scores: Array[Int], frame: Int, newRoll: Boolean, beforeLast: Char, last: Char, frames: List[Char]): Int = {
    def updatePrevious(updateValue: Int) = {
      if ((last == 'X' || last == '/') && frame > 0 && frame - 1 < 10)
        scores(frame - 1) = scores(frame - 1) + updateValue
      if (beforeLast == 'X' && frame > 1) {
        val previousFrame = if(newRoll == true) frame - 2 else frame - 1
        scores(previousFrame) = scores(previousFrame) + updateValue
      }
    }

    frames match {
      case Nil => scores.sum
      case _ => {
        frames.head match {
          case 'X' =>
            if (frame < 10)
              scores(frame) = 10
            updatePrevious(10)
            process(scores, frame + 1, true, last, 'X', frames.tail)
          case '-' =>
            val newFrame = if (newRoll) frame else if (frame == 9) frame else frame + 1
            process(scores, newFrame, !newRoll, last,'-', frames.tail)
          case '/' =>
            scores(frame) = 10
            updatePrevious(10-last)
            process(scores, frame + 1, true, last, '/', frames.tail)
          case n if (n >= '1' || n <= '9') =>
            if (frame < 10)
              scores(frame) = scores(frame) + n.toString.toInt
              updatePrevious(n.toString.toInt)
            val newFrame = if (newRoll) frame else if (frame == 9) frame else frame + 1
            process(scores, newFrame, !newRoll, last, n, frames.tail)
          case x => throw new IllegalArgumentException(s"Invalid character $x")
        }
      }
    }

  }

  def score(frames: String): Int = {
    val scores: Array[Int] = Array.ofDim(10)
    frames.length match {
      case len if len < 10 => throw new IllegalArgumentException(s"invalid game with $len frames")
      case len if len > 22 => throw new IllegalArgumentException(s"invalid game with $len frames")
      case _@len => process(scores, 0, true, ' ', ' ', frames.toCharArray().toList)
    }

  }

}
