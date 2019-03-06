

import scala.math._

object TD1 {

  def main(args: List[String]): Unit = {
        //println(f1(1,2))
        //println(f2(1,2))
        //println(num)
        //println(cardList(num))
        //println(prodTerm(1,3))
        //println(8*pi(20))
        //println(sinC(1))
        //println(Ex1_plus(1,4))
        //println(Ex1_mult(1,4))
        //println(integrale(id,110)(0,2))
        //println(integrale(sqrt,100)(0,10))
        println (recfact(4))
      }
  
  
  
  
  // il y a une etape en plus entre l'evaluation de f1 et f2
  // f2 est directement interpreté par le corps de la fonction comme si c'était deja precompilé
  // f1 remplace la definition de la fonction dans l'expression appelante PUIS execute le corps de la fonction
  // f1 est une fonction, f2 est une méthode. Avec def on n'est pas obligé de calculer le type de retour
  def f1(x:Int, y:Int):Int={ x+y }
  val f2 = (x:Int, y:Int) => x+y

  
  // artité d'une fonction f(x:int, y:int, z:int):Int est de 1! la fonction prend un triplet de valeurs de N et renvoie un entier.
  // curryfier <=> extraire un param puis renvoyer une fonction avec les param restant : (int)=>((int, int) => int) = {x => { (y,z) => x+y+z }}
  
  // Liste:
  val num = 1::2::3::Nil
  // methode disponibles: num.head , num.tail (la liste sans le premier element (sens =>)) , num.isEmpty
  
  def cardList(l:List[Int]):Int={
    if(l.isEmpty)
      0
    else
      1 + cardList(l.tail)
  }
  
  // Ex 1.1
  def prod(a:Int, b:Int ):Int ={
    if(a>b)
      a*prod(a-1,b)
    else if (b>a)
      b*prod(a,b-1)
    else
      a
  }
  
  // Ex 1.2
  def prodTerm (a:Int, b:Int)={
    def subProd(a:Int, b:Int, acc:Int ):Int ={
    if(a>b)
      subProd(a-1, b, a*acc)
    else if (b>a)
      subProd(a, b-1, b*acc)
    else
      acc
    }
    subProd(a,b,1)
    }
  
  // Ex 1.3
  def pi(n:Int):Double={
    if (n>0){
      pi(n-1)+1/((4*n.toDouble+1)*(4*n.toDouble+3))
    }
    else
      1.0/3.0
  }
  
  // Ex 1.4

  def compose[A,B,C](f:B => A, g: C => B):(C => A) = {
    { (x:C) => f(g(x))}
  }
  
  val sinC = compose(sin,{(x:Double) => 2*x.toDouble+1})
  
  // Ex 1.5: + { (x:Int,y:Int) => x+y}
  
  var plus = { (x:Int,y:Int) => x+y} 
  var mult = {(x:Int,y:Int) => x*y }
  
  def fab(f:(Int,Int)=>Int, x:Int, y:Int):Int={
    if(x>y)
      f(x, fab(f,x-1,y))
    else if (y>x)
      f(y, fab(f,x,y-1))
    else
      x
  }
  
  var Ex1_plus = { (x:Int,y:Int) => fab(plus,x,y) }
  var Ex1_mult = { (x:Int,y:Int) => fab(mult,x,y) }
 
  // Ex 1.6
  def integrale(f:Double => Double, n:Int):(Double, Double) => Double ={
    def subint(a:Double, b:Double, step:Double):Double ={
      if (step<b){
        f(step)*((b-a)/n) + subint(a,b,step+(b-a)/n)
      }
      else{
        0
      }
    }
    
    {(a:Double, b:Double) => subint(a,b,a)}
  }
  
  val id = {(x:Double) => x }
  
  // Ex 1.7
  
  def genrec(base:Int => Boolean, acc: Int, compose: (Int, Int) => Int, decrois: Int => Int) : Int => Int ={
    
    def subrec(n:Int, acc:Int):Int={
      if (base(n))
        acc
      else{
        subrec (decrois(n), compose(n, acc))
      }
    }
    { (n:Int) => subrec(n,acc)  }
  }
  
  def valbase = 1
  def base = {(x:Int) => x==valbase}
  def compose2 = { (x:Int, y:Int) => x*y }
  def decrois = {(x:Int) => x-1 }
  
  def recfact = genrec(base, valbase, compose2, decrois)
  
}