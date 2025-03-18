type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  def transpose: Matrix =
    data match {
      case None => Matrix(None)
      case Some(x) =>
        def auxTranspose(x: Mat): Mat =
          x match {
            case Nil :: _ => Nil
            case _ => x.map(_.head) :: auxTranspose(x.map(_.tail))
          }

        Matrix(auxTranspose(x))
    }
  def map(f: Double => Double): Matrix = {
    data match {
      case None => Matrix(None)
      case Some(m) => Matrix(m.map(_.map(f)))
    }
  }
  def *(other: Matrix): Matrix = {
    if (other.height != width) Matrix(None)
    else (data, other.data) match {
      case (None, _) => Matrix(None)
      case (_, None) => Matrix(None)
      case (Some(m), Some(n)) =>
        Matrix(m.map(line => n.transpose.map(col => line.zip(col)
          .map(pair => pair._1 * pair._2).sum)))
    }
  }
  def ++(x: Double): Matrix = {
    data match {
      case None => Matrix(None)
      case Some(m) => Matrix(m.map(_:+ x))
    }
  }
  def -(other: Matrix): Matrix = {
    if ((other.height != height) || (other.width != width)) Matrix(None)
    else (data, other.data) match {
      case (Some(m), Some(n)) =>
        Matrix(m.zip(n).map((line1, line2) => line1.zip(line2).map((a, b) => a - b)))
      case _ => Matrix(None)
    }
  }

  def data: Option[Mat] = m
  def height: Option[Int] =
    data match {
      case None => None
      case Some(x) => Some(x.length)
    }
  def width: Option[Int] =
    data match {
      case None => None
      case Some(x) => Some(x.head.length)
    }

  override def toString: String =
    data match {
      case None => None.toString
      case Some(x) => x.map(_.mkString(",")).mkString("\n")
    }
}


  object Matrix {
  def apply(data: Mat): Matrix =
    data match {
      case Nil => new Matrix(None)
      case _ =>  new Matrix(Some(data))
    }
  def apply(data: Option[Mat]): Matrix =
    data match {
      case None => new Matrix(None)
      case Some(_) => new Matrix(data)
    }

  def apply(dataset: Dataset): Matrix = new Matrix(Some(dataset.data.tail.map(_.map(_.toDouble))))

}
