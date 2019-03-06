import scala.math._

object pre_partiel_1 {
  
  def main(args: Array[String]): Unit = {
    //println(insert(List(1,1,2,4),3))
    //println(triInsertion(List(1,1,2,4),List(2,3)))
    //println(8*leibniz(45))
    //println(8*leibniz2(45))
    //println(cSin(8))
    //println(checkSin(8))
    //println(fab({(x:Int,y:Int)=>x*y},1,5))
    //println(msort(List(5,2,8,6,7,3)))
    //println(split(List(1,2,3,4),2))
    //println(ajouteAToute(20,List(List(1,2),List(2,3))))
    println(parties(List(1,2,3)))
  }
  
  
  
  def insert(l:List[Int], x:Int):List[Int] ={
    if(l.isEmpty)
      List(x)
    else {
      if(x>l.head)
        l.head :: insert(l.tail,x)
      else
        x :: l
    }
  }
  
 def triInsertion(l1:List[Int],l2:List[Int]):List[Int] ={
   if(l1.isEmpty)
     l2
   else
     triInsertion(l1.tail,insert(l2,l1.head))
 }
 
def triInsert(l1:List[Int]):List[Int] ={
  triInsertion(l1,List())
}

val a = 2
val tif = if (a == 2) true else false

def affine(a:Int, b:Int):Double => Double ={
  {(x:Double) => a*x+b}
}
 
def puissance(x:Int, n:Int):Double ={
  if(n>0){
    x* puissance(x,n)
  }
  else
    1
}

def puissance2(x:Int, n:Int, acc:Double):Double ={
  if(n>0){
    puissance2(x,n,x*x)
  }
  else
    acc
}

def leibniz(n:Int):Double ={
  def sub(np:Int,acc:Double):Double={
  if(np<=2*n)
    sub(np+2,acc + 1/((2*np.toDouble+1)*(2*np.toDouble+3)))
  else
    acc
  }
  sub(0,0)
}

def leibniz2(n:Int):Double ={
  def sub(np:Int,acc:Double):Double={
  if(np<=4*n+1)
    sub(np+4,acc + 1/(np.toDouble*(np.toDouble+2)))
  else
    acc
  }
  sub(1,0)
}

def compose[A,B,C](f:B=>C, g:A=>B):A=>C={
  (x:A) => f(g(x))
}

def cSin = compose(sin,{(x:Double)=>2*x+1})
def checkSin(x:Double):Double ={sin(2*x+1)} 

def fab(f:(Int,Int)=>Int, a:Int,b:Int):Int ={
  if(a<b)
    f(a,fab(f,a+1,b))
  else
    b
}

def split(lp:List[Int],n:Int):(List[Int],List[Int])={
    def sub(lpp:List[Int],l1:List[Int],l2:List[Int],ns:Int):(List[Int],List[Int])={
     if(lpp.isEmpty)
        (l1, l2)
    else 
      if(ns>0)
        sub(lpp.tail, lpp.head :: l1, l2, ns-1)
      else
        sub(lpp.tail, l1, lpp.head :: l2, ns-1)
    }
    sub(lp,List(),List(),n)
  }

def split2(lp:List[Int],n:Int):(List[Int],List[Int])={
  
  if(lp.isEmpty){
    (Nil,Nil)
  }
  else{
    val (ll,lr) = split(lp.tail,n-1)
    if(n>0)
      (lp.head :: ll, lr)
    else
      (ll, lp.head :: lr)
  }
  }

def msort(l:List[Int]):List[Int]={
  
def split(lp:List[Int],n:Int):(List[Int],List[Int])={
  
  if(lp.isEmpty){
    (Nil,Nil)
  }
  else{
    val (ll,lr) = split(lp.tail,n-1)
    if(n>0)
      (lp.head :: ll, lr)
    else
      (ll, lp.head :: lr)
  }
  }
  
  def merge(l1:List[Int],l2:List[Int]):List[Int]={
    if(l1.isEmpty)
      l2
    else if (l2.isEmpty) l1
    else if (l1.head<l2.head) l1.head :: merge(l1.tail,l2)
    else l2.head :: merge(l1, l2.tail)
  }
  
  val mid = l.length/2
  if(mid>0){
    val (ll, lr) = split (l, mid)
    merge(msort(ll),msort(lr))
  }
  else
    l 
}

def ajouteAToute[A](x:A, l:List[List[A]]):List[List[A]] ={
  if(l.isEmpty){
    l
  }
  else
    (x::l.head)::ajouteAToute(x,l.tail)
}

def parties[A](l:List[A]):List[List[A]] ={
  
  def sub(lp:List[A], res:List[List[A]]):List[List[A]]={
    if(lp.isEmpty){
      res
    }
    else
      sub(lp.tail, res ::: ajouteAToute(lp.head,res))
  }
  
  sub(l, List(List()))
  
}

}