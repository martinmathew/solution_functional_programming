import scalaz._
import Scalaz._

((x:Int) => x+1)map{_*7}
1.point[List]