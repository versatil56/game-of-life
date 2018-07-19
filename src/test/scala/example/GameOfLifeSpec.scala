package example

import org.scalatest.{FlatSpec, _}

class GameOfLifeSpec extends FlatSpec with MustMatchers {

  behavior of "A Cell"

  it should "be dead by default" in {
    val cell = Cell(coordinates = AddressCoordinates())

    cell.isAlive mustBe false
  }

  it should "be able to be alive" in {
    val cell = Cell(true, AddressCoordinates())

    cell.isAlive mustBe true
  }

  it should "be able to die" in {
    val cell = Cell(true, AddressCoordinates())

    cell.die() mustBe Cell(coordinates = AddressCoordinates())
  }

  it should "be able to resucitate" in {
    val cell = Cell(coordinates = AddressCoordinates())
    cell.resucitate() mustBe Cell(true, AddressCoordinates())
  }

  it should "have an address" in {
    val cell = Cell(coordinates = AddressCoordinates())

    cell.coordinates mustBe AddressCoordinates()
  }

  behavior of "The World"

  it should "contain cells" in {
    val world = World(List(Cell(coordinates = AddressCoordinates()), Cell(coordinates = AddressCoordinates())))

    world.cells mustBe Seq(Cell(coordinates = AddressCoordinates()), Cell(coordinates = AddressCoordinates()))
  }

  it should "contain a size" in {
    val world = World(List(Cell(coordinates = AddressCoordinates()), Cell(coordinates = AddressCoordinates())), 3)

    world.size mustBe 3
  }

  it should "be able to initialise and set the right number of cells given a size" in {
    val world = World(size = 5)

    val output = world.initialise()

    output.cells.size mustBe 25
  }

  behavior of "Checking my neighbors"

  it should "say that I have eight alive neighbors when I am in the middle of a 3x3" in {
    val cell0 = Cell(true,AddressCoordinates(0,0))
    val cell1 = Cell(true,AddressCoordinates(1,0))
    val cell2 = Cell(true,AddressCoordinates(2,0))
    val cell3 = Cell(true,AddressCoordinates(0,1))
    val cell4 = Cell(true,AddressCoordinates(1,1))
    val cell5 = Cell(true,AddressCoordinates(2,1))
    val cell6 = Cell(true,AddressCoordinates(0,2))
    val cell7 = Cell(true,AddressCoordinates(1,2))
    val cell8 = Cell(true,AddressCoordinates(2,2))

    val cells = List(cell0,cell1,cell2,cell3,cell4,cell5,cell6,cell7,cell8)
    val middleCell = cells(4)


    middleCell.checkMyNeighbors(cells).size mustBe 8
  }

  it should "say that I have three alive neighbors when I am in an edge" in {
    val cell0 = Cell(true,AddressCoordinates(0,0))
    val cell1 = Cell(true,AddressCoordinates(1,0))
    val cell2 = Cell(true,AddressCoordinates(0,1))
    val cell3 = Cell(true,AddressCoordinates(1,1))

    val cells = List(cell0,cell1,cell2,cell3)

    val edgeCell = cells(0)

    edgeCell.checkMyNeighbors(cells).size mustBe 3
  }

  it should "say that I have no neighbors if all of them are dead" in {
    val world = World(size = 2).initialise()

    val edgeCell = world.cells(0)

    edgeCell.checkMyNeighbors(world.cells).size mustBe 0
  }

  behavior of "Rules of Live"

  it should "kill a cell with fewer than two alive neighbors" in {
    val cell0 = Cell(true,AddressCoordinates(0,0))
    val cell1 = Cell(true,AddressCoordinates(1,0))
    val cell2 = Cell(false,AddressCoordinates(0,1))
    val cell3 = Cell(false,AddressCoordinates(1,1))

    val cells = List(cell0,cell1,cell2,cell3)

    val edgeCell = cells(0)
    val aliveNeighbors = edgeCell.checkMyNeighbors(cells).size

    edgeCell.assessLifeConditions(aliveNeighbors) mustBe Cell(false,AddressCoordinates(0,0))
  }

  it should "kill a cell with more than three alive neighbors" in {
    val cell0 = Cell(true,AddressCoordinates(0,0))
    val cell1 = Cell(true,AddressCoordinates(1,0))
    val cell2 = Cell(true,AddressCoordinates(2,0))
    val cell3 = Cell(true,AddressCoordinates(0,1))
    val cell4 = Cell(true,AddressCoordinates(1,1))
    val cell5 = Cell(true,AddressCoordinates(2,1))
    val cell6 = Cell(true,AddressCoordinates(0,2))
    val cell7 = Cell(true,AddressCoordinates(1,2))
    val cell8 = Cell(true,AddressCoordinates(2,2))

    val cells = List(cell0,cell1,cell2,cell3,cell4,cell5,cell6,cell7,cell8)
    val middleCell = cells(4)
    val aliveNeighbors = middleCell.checkMyNeighbors(cells).size

    middleCell.assessLifeConditions(aliveNeighbors) mustBe Cell(false,AddressCoordinates(1,1))
  }

  it should "leave cell alive if 2 alive neighbors" in {
    val cell0 = Cell(true,AddressCoordinates(0,0))
    val cell1 = Cell(true,AddressCoordinates(1,0))
    val cell2 = Cell(true,AddressCoordinates(0,1))
    val cell3 = Cell(false,AddressCoordinates(1,1))

    val cells = List(cell0,cell1,cell2,cell3)

    val edgeCell = cells(0)
    val aliveNeighbors = edgeCell.checkMyNeighbors(cells).size

    edgeCell.assessLifeConditions(aliveNeighbors) mustBe Cell(true,AddressCoordinates(0,0))
  }

  it should "leave cell alive if 3 alive neighbors" in {
    val cell0 = Cell(true,AddressCoordinates(0,0))
    val cell1 = Cell(true,AddressCoordinates(1,0))
    val cell2 = Cell(true,AddressCoordinates(0,1))
    val cell3 = Cell(true,AddressCoordinates(1,1))

    val cells = List(cell0,cell1,cell2,cell3)

    val edgeCell = cells(0)
    val aliveNeighbors = edgeCell.checkMyNeighbors(cells).size

    edgeCell.assessLifeConditions(aliveNeighbors) mustBe Cell(true,AddressCoordinates(0,0))
  }

  it should "bring dead cell back to live if there is exactly 3 alive neighbors" in {
    val cell0 = Cell(false,AddressCoordinates(0,0))
    val cell1 = Cell(true,AddressCoordinates(1,0))
    val cell2 = Cell(true,AddressCoordinates(0,1))
    val cell3 = Cell(true,AddressCoordinates(1,1))

    val cells = List(cell0,cell1,cell2,cell3)

    val edgeCell = cells(0)
    val aliveNeighbors = edgeCell.checkMyNeighbors(cells).size

    edgeCell.assessLifeConditions(aliveNeighbors) mustBe Cell(true,AddressCoordinates(0,0))
  }

  behavior of "Address"

  it should "return (0,0) when you are the first element" in {
    val address = Address(0, 8)

    address.get mustBe AddressCoordinates()
  }

  it should "return (1,0) when you are at the second element" in {
    val address = Address(1, 2)

    address.get mustBe AddressCoordinates(1, 0)
  }

  it should "return (3,0) when you are in the middle of first row in 6 size" in {
    val address = Address(3, 6)

    address.get mustBe AddressCoordinates(3, 0)
  }

  it should "return (5,0) when you are in the edge of first row in 6 size" in {
    val address = Address(5, 6)

    address.get mustBe AddressCoordinates(5, 0)
  }

  it should "return (0,1) when on first element of second row" in {
    val address = Address(6, 6)

    address.get mustBe AddressCoordinates(0, 1)
  }

  it should "return (0,5) when on last row on size 6" in {
    val address = Address(30, 6)

    address.get mustBe AddressCoordinates(0, 5)
  }

  it should "return (2,2) when on bottom right edge of a size 3" in {
    val address = Address(8, 3)

    address.get mustBe AddressCoordinates(2, 2)
  }

  it should "return (5,5) when on bottom right edge of a size 6" in {
    val address = Address(35, 6)

    address.get mustBe AddressCoordinates(5, 5)
  }
}

case class AddressCoordinates(x: Int = 0, y: Int = 0)

case class Address(element: Int = 0, size: Int = 0) {
  private def getX(): Int = {
    val column = element - (getY() * size)
    if (column < size) column else 0
  }

  private def getY(): Int = {
    if (element < size) 0 else {
      val row = element / size
      row
    }
  }

  def get() = AddressCoordinates(getX(), getY())

}

case class World(cells: List[Cell] = List(), size: Int = 0) {
  def initialise() :World = {
    val cells = populateWorld(0,size,List())
    World(cells,size)
  }

  @scala.annotation.tailrec
  private def populateWorld(element: Int, size: Int, cells: List[Cell]): List[Cell] ={
    if (element < size*size){
      val address = Address(element,size)
      populateWorld(element + 1,size,cells :+ Cell(coordinates = address.get()))
    } else cells
  }
}

case class Cell(isAlive: Boolean = false, coordinates: AddressCoordinates) {
  def die() = Cell(false,coordinates)

  def resucitate() = Cell(true,coordinates)

  def stayAsYouAre() = Cell(isAlive,coordinates)

  def checkMyNeighbors(cells: Seq[Cell] = Seq()) = {
    val potentialNeighborAddresses = Seq((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))

    potentialNeighborAddresses.flatMap(potentialNeighborAddress => {
      val neighborAddress = AddressCoordinates(potentialNeighborAddress._1 + coordinates.x, potentialNeighborAddress._2 + coordinates.y)
      cells.find(_.coordinates == neighborAddress)
    }).filter(_.isAlive)
  }

  def assessLifeConditions(numberOfAliveNeighbors: Int) : Cell = {
    numberOfAliveNeighbors match {
      case number if number < 2 && isAlive => die()
      case number if number > 3 && isAlive => die()
      case number if number == 3 && !isAlive => resucitate()
      case _ => stayAsYouAre()
    }
  }
}
