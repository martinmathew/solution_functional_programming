import scalaz._

trait Monoid[A] {

  def mappend(a1:A,a2:A):A
  def mzero: A
}

object Monoid {
  implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
    override def mappend(a1: Int, a2: Int): Int = a1+a2

    override def mzero: Int = 0
  }

  implicit val StringMonoid:Monoid[String] = new Monoid[String] {
    override def mappend(a1: String, a2: String): String = a1+a2

    override def mzero: String = ""
  }

}

def sum[A:Monoid](xs:List[A]):A ={
  val m = implicitly[Monoid[A]]
  xs.foldLeft(m.mzero)(m.mappend)
}

object FoldLeftList {

  def foldLeft[A,B](xs:List[A],b:B,f:(B,A)=>B) = xs.foldLeft(b)(f)
}


def sum1[A:Monoid](xs:List[A]):A = {
  val s = implicitly[Monoid[A]]
  FoldLeftList.foldLeft(xs,s.mzero,s.mappend)
}

trait MonoidOp[A]{
  val F: Monoid[A]
  val value:A
  def |+|(a2:A)={
    print("A2 : ",a2)
    F.mappend(value,a2)
  }
}

implicit def tojunk[A:Monoid](a:A):MonoidOp[A] = new MonoidOp[A] {
  override val F: Monoid[A] = implicitly[Monoid[A]]
  override val value: A = a

}

3 |+| 90