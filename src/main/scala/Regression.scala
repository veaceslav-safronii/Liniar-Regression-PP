object Regression {

  def regression(
                  dataset_file: String,
                  attribute_columns: List[String],
                  value_column: String,
                  test_percentage: Double,
                  alpha: Double,
                  gradient_descent_steps: Int
                ): (Matrix, Double) = {
    val dataset = Dataset(dataset_file)
    val (train, test) = dataset.selectColumns(attribute_columns :+ value_column)
      .split(test_percentage)
    val X_train: Matrix = Matrix(train.selectColumns(attribute_columns)) ++ 1
    val W = Matrix(List(List.fill(X_train.width.get)(0.0))).transpose
    val Y_train = Matrix(train.selectColumn(value_column))

    def calculate(steps: Int, W: Matrix): Matrix = {
      if (steps == 0) W
      else {
        val estimates = X_train * W
        val errors = estimates - Y_train
        val gradient = (X_train.transpose * errors).map(_./(X_train.height.get))
        calculate(steps - 1, W - gradient.map(_ * alpha))
      }
    }

    val coeff = calculate(gradient_descent_steps, W)
    val X_test = Matrix(test.selectColumns(attribute_columns)) ++ 1
    val Y_test = Matrix(test.selectColumn(value_column))

    val error = (X_test * coeff - Y_test).data.get.transpose.head.sum / X_test.height.get
    (coeff, error)
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}