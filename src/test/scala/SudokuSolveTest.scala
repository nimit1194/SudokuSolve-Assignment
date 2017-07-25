import org.scalatest.FunSuite

class SudokuPuzzleTest extends FunSuite {

  val grid = Vector(Vector(0,0,4,8,0,0,0,1,7), Vector(6,7,0,9,0,0,0,0,0), Vector(5,0,8,0,3,0,0,0,4),
    Vector(3,0,0,7,4,0,1,0,0), Vector(0,6,9,0,0,0,7,8,0), Vector(0,0,1,0,6,9,0,0,5),
    Vector(1,0,0,0,8,0,3,0,6), Vector(0,0,0,0,0,6,0,9,1), Vector(2,4,0,0,0,1,5,0,0))

  val gridWithZeros = Vector(Vector(0,0,0,0,0,0,0,0,0), Vector(0,0,0,0,0,0,0,0,0), Vector(0,0,0,0,0,0,0,0,0),
    Vector(0,0,0,0,0,0,0,0,0), Vector(0,0,0,0,0,0,0,0,0), Vector(0,0,0,0,0,0,0,0,0),
    Vector(0,0,0,0,0,0,0,0,0), Vector(0,0,0,0,0,0,0,0,0), Vector(0,0,0,0,0,0,0,0,0))

  val gridFail = Vector(Vector(1,1,4,8,0,0,0,1,7), Vector(6,7,0,9,0,0,0,0,0), Vector(5,0,8,0,3,0,0,0,4),
    Vector(3,0,0,7,4,0,1,0,0), Vector(0,6,9,0,0,0,7,8,0), Vector(0,0,1,0,6,9,0,0,5),
    Vector(1,0,0,0,8,0,3,0,6), Vector(0,0,0,0,0,6,0,9,1), Vector(2,4,0,0,0,1,5,0,0))

  val sudoObj = new SudokuSolve

  test("Testing for sudoku where solution exists")
  {
    assert(sudoObj.sudokuSolver(grid))
  }

  test("Testing for sudoku with all 0s")
  {
    assert(sudoObj.sudokuSolver(gridWithZeros))
  }

  test("Testing for sudoku where solution doesn't exist")
  {
    assert(sudoObj.sudokuSolver(gridFail) === false)
  }
}