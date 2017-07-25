
class SudokuSolve {

  def sudokuSolver(grid: Vector[Vector[Int]]): Boolean = {

    def solver(grid: Vector[Vector[Int]], row:Int, col:Int, prevVal:Int = 0): Boolean = {
      if (row == 9) {println(grid.map(_.mkString(" ")).mkString("\n") + "\n")
        true}
      else if (col == 9) {solver(grid, row + 1, 0)}
      else {
        val (row,col) = findUnassignedLoc(grid)
        if(row == -1 && col == -1)  {true}
        else {
          val num = whatToAssign(grid, row, col,prevVal)
          if (num == -1) {false}
          else {
            val updatedGrid = grid.updated(row, grid(row).updated(col, num))
            if (solver(updatedGrid, row, col + 1)) {true} else {solver(grid, row, col, updatedGrid(row)(col))}
          }
        }
      }
    }
    solver(grid, 0,0)

  }

  private def whatToAssign(grid: Vector[Vector[Int]], row: Int, col: Int, prevVal:Int): Int = {

    val upperLimit = 10
    val num = List.range(prevVal + 1,upperLimit)
    def assign(grid: Vector[Vector[Int]], num: List[Int], row: Int,col: Int): Int = {
      if(num.isEmpty) {-1}
      else if(isSafeToAssign(grid, row, col, num.head)) {num.head}
      else {assign(grid, num.tail, row, col)}
    }
    assign(grid, num, row,col)

  }

  private def findUnassignedLoc(grid: Vector[Vector[Int]]): (Int,Int) = {

    def find(grid: Vector[Vector[Int]], row: Int, col: Int): (Int, Int) = {

      if(grid(row)(col) == 0) {(row, col)}
      else
      {
        if(row ==8 && col == 8) {(-1,-1)} else if(col == 8) {find(grid, row + 1, 0)} else {find(grid, row, col + 1)}
      }
    }
    find(grid,0,0)
  }

  private def isSafeToAssign(grid: Vector[Vector[Int]], row: Int, col: Int,num:Int): Boolean = {

    if(conflictInRow(grid,row,num) || conflictInCol(grid,col,num) || conflictInBox(grid,row-row%3,col-col%3,num))
    {false}
    else {true}
  }

  private def conflictInRow(grid: Vector[Vector[Int]], row:Int, num:Int): Boolean = {

    if(grid(row).contains(num)) true else false
  }

  private def conflictInCol(grid: Vector[Vector[Int]], col:Int, num:Int): Boolean = {

    val colVector = grid.map(_(col))
    if(colVector.contains(num)) true else false
  }

  private def conflictInBox(grid: Vector[Vector[Int]], row:Int, col:Int, num:Int): Boolean = {

    def conflict(grid: Vector[Vector[Int]], row:Int, col:Int, num:Int, i:Int, j:Int): Boolean = {

      if(j==3) {false}
      else if (i==3) {conflict(grid, row, col, num, 0, j + 1)}
      else{
        if(grid(row + i)(col + j) == num)
        {true}
        else
        {conflict(grid,row,col,num,i + 1,j)}
      }
    }
    conflict(grid, row, col, num, 0, 0)

  }

}