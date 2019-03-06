

object TD3 {
   def main(args: Array[String]): Unit = {
      //println( has_enough_parenthesis("(bidule))"))
      //println(has_enough_parenthesis("(x+2))("))
     //println( countChange(4,List(1,2)))
     //println( twist(List((1,2),(3,4))) )
     //println( lengthList(List(1,2,3,4)) )
     //println( concat(List(1,2,3),List(4,5,6)) )
     //println(flat(List(List(1,2,3),List(1,2,3),List(1,2,3))))
     //println(gardetantque(List(2,3,4,5),{(x:Int)=> x%2==0}))
     //println(termexp(2,3))
     //println(somme1(2,3))
     //println(flip(List(1,2,3,4,5)))
     //println(test28(1,5))
     //println(tousVrai(List(1,2,3),{(x:Int)=>x<3}))
     //println(multiples(2,11))
     //println(estPremier(29))
     //println(quicksort(List(15,16,6,12,1,0)))
     //println(estDans(List(15,16,6,12,1,0),22))
     //println(estDans2(2,List('a', 3, List(4, List("toto", 2), 3.14), 18)))
     println(applatir(List('a', 3, List(4, List("toto", 2), 3.14), 18)))
     //println(new test(1,8).el1)

   }


// Ex 3.1
   
def count_parenthesis (l:List[Char], acc:Int):Int ={
  if(!l.isEmpty){
    if(acc>=0){
    if(l.head == '(')
      count_parenthesis(l.tail,acc+1)
    else if (l.head == ')')
      count_parenthesis(l.tail,acc-1)
    else
      count_parenthesis(l.tail,acc)
    }
    else
      acc
  }
  else
    acc
}

def has_enough_parenthesis(s:String):Boolean ={
  if(count_parenthesis(s.toList,0)!=0)
    false
  else
    true
}

// Ex 3.2
def countChange(montant: Int, pieces: List[Int]): Int = {
  if(montant == 0)
    1
  else if(montant > 0 && !pieces.isEmpty)
    countChange(montant - pieces.head, pieces) + countChange(montant, pieces.tail)
  else
    0
}
//def change(montant:Int, pieces:List[Int]):Int ={
  
//}


/* def map2[A](l:List[A], f: A => A):List[A] ={
  if(!l.isEmpty)
    f(l.head) :: map2(l.tail, f)
  else
    l
}  */

// Ex 3.3 
def twist[A](l:List[(A,A)]):List[(A,A)] ={
  l.map({ x:(A,A) => (x._2,x._1)})
    // on aurait aussi pu utiliser: val (y,z) = x
}

// Ex 3.4
def lengthList[A](l:List[A]):Int ={
  l.foldRight(0)({(x,y) => 1+y })
}

// Ex 3.5
def concat[A](l1:List[A], l2:List[A]):List[A] ={
  l1.foldRight(l2)({ (x,y) => x::y })
}

// Ex 3.6
def flat[A](l:List[List[A]]):List[A] ={
  l.foldRight(List[A]())({(x,y) => concat(x,y)})
}

// Ex 3.7
def tousVrai(l:List[Int], f:Int => Boolean):Boolean={
  l.foldRight(true)((x,y)=>f(x) && y)
}

// Ex 3.8
def multiples(a:Int, b:Int):List[Int]={
  val l = List.range(1,b)
  l.filter(_ % a ==0)
}

// Ex 3.9
def estPremier(n:Int):Boolean={
  val l = List.range(2,n)
  val lf = l.filter(n % _ ==0)
  lf.isEmpty
}

// Ex 3.10
def quicksort(l:List[Int]):List[Int] ={
  if(l.length >= 2){
    val m = (l.length/2)-1
    quicksort(l.filter(_<l(m))) ++ quicksort(l.filter(_ == l(m))) ++ quicksort(l.filter(_>l(m)))
  }
  else
    l
}

/* def sortFunctional(xs: List[Int]): List[Int] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      println(pivot)
      println(xs filter (pivot >))
      List.concat(sortFunctional(xs filter (pivot >)), xs filter (pivot ==), sortFunctional(xs filter (pivot <)))
    }
  } */

// Ex 3.11
def estDans(l:List[Int], x:Int):Boolean ={
  val lf = l.filter(_ == x)
  !lf.isEmpty
}

// Ex 3.12

// List('a', 3, List(4, List("toto", 2), 3.14), 18)
def estDans2(x:Any, l:List[Any]): Boolean = l match {
    case (n::r) :: rest => estDans2(x, n::r) || estDans2(x,rest)
    case np::rp => if(x == np) true  else estDans2(x,rp)
    case Nil => false
  }

// Ex 3.13
def applatir(l:List[Any]): List[Any] = l match {
    case (n::r) :: rest => applatir(n::r) ::: applatir(rest)
    case np::rp => np :: applatir(rp)
    case Nil => Nil
  }


class test(x:Int, y:Int){
  require(x>0,"x doit être superieur à 0")
  def el1 = x
  def el2 = y
}
// notation infixe possible si un seul param: test.plus(a) <=> test plus a
// possibilité de nommer une fonction avec des symboles (+,-...): test + a
// pour afficher un res, definir toString dans la classe


}