import scala.io.Source

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m
  override def toString: String = data
    .map(_.mkString(","))
    .mkString("\n")

  def selectColumn(col: String): Dataset = {
    Dataset(data.transpose.filter(_.head == col).transpose)
  }
  def selectColumns(cols: List[String]): Dataset = {
    Dataset(cols.map(col => data.transpose.filter(_.head == col).head).transpose)
  }

  def split(percentage: Double): (Dataset, Dataset) = {
    val sortedSet = getRows.sortBy(_.head)
    val (train, test) = sortedSet
      .foldLeft((List.empty[List[String]], List.empty[List[String]]))((acc, x) =>
        if ((acc._1.length + acc._2.length + 1) % (1/percentage).ceil == 0) (acc._1, x::acc._2)
        else (x::acc._1, acc._2))

    (Dataset(getHeader :: train.reverse), Dataset(getHeader :: test.reverse))

  }

  def size: Int = data.tail.length
  def getRows: List[List[String]] = data.tail
  def getHeader: List[String] = data.head
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val file = Source.fromFile(csv_filename)
    val m = file.getLines().map(_.split(",").map(_.trim).toList).toList
    file.close()
    new Dataset(m)
  }


  def apply(ds: List[List[String]]): Dataset = new Dataset(ds)
}
