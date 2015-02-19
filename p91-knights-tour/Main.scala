object Main extends App {

  val k = Knight(5)

  k.all.map { start =>
    val root = List(List(start))

    k.results = List()
    k.step(root)

    val best = k.results.sortWith((a, b) => a.length > b.length).head
    println(s"Starting form $start got ${k.results.length} solutions, path of ${best.length} moves/${k.boardSize*k.boardSize} (missing ${k.all.diff(best).length}) - Path is : $best: missings are : ${k.all.diff(best)}")

    //res.map(println)
  }
//  val c = List(Node(Point(2, 2), List(), Some(root)), Node(Point(3, 3), List(), Some(root)))
//  root.copy(children=c)
//  println(c(0).path)
}

case class Knight(boardSize: Int) {
  def possibles(p: Point, visited: List[Point]) = movments.map(p+_).filter(pt => pt.x >= 0 && pt.y >= 0 && pt.x < boardSize && pt.y < boardSize).distinct.diff(visited)

  def all = Range(0, boardSize).toList.map( x => Range(0, boardSize).toList.map( y => Point(x, y) ) ).flatten

  var results : List[List[Point]] = List()

  def step(l: List[List[Point]]) : List[List[Point]] = {
    //println(l)
    val nlst : List[(List[Point], List[Point])]= l.map {
      subList => (subList, subList.lastOption.map( last => possibles(last, subList)).getOrElse(List()))
    }.filterNot(_._1.isEmpty)

    //println(l, "..........", nlst, nlst.diff(l) == nlst)
    //nlst.map(p => { step(l:::List(p)) })
    //

    //println("NEXT MOV => ", nlst.length)
    nlst.map { l =>
      //println(s"got ${l._2.length} solutions")
    }

    val r : List[List[Point]] = nlst.map { next =>
      next match {
        case (sl: List[Point], List()) =>
        {
          this.results = this.results:::List(sl)

          List(
            if(all.diff(sl).isEmpty) {
              println("GOT IT!!!!!!", sl)
              sl
            }
            else List()
          )
        }
        case (sl: List[Point], n: List[Point]) => {
          n.map {
            p => { sl:::List(p) }
          }
        }
      }
    }.flatten

    //println(s" R IS ${r.length} elements long",  r)

    r.filterNot(_.isEmpty) match {
      case List() => List()
      case _ => step(r)
    }
  }

  val movments = List(
    Point(2, -1), Point(2, 1),
    Point(-2, -1), Point(-2, 1),
    Point(1, -2), Point(1, 2),
    Point(-1, -2), Point(-1, 2)
  )
}

case class Result(path: List[Point], a: Option[Long] = None)

case class Point(x: Integer, y: Integer) {
  def +(p: Point) = Point(x+p.x, y+p.y)
  def -(p: Point) = Point(x-p.x, y-p.y)
  override def toString = s"[$x,$y]"
}

