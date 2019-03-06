

object TD4 {
     def main(args: Array[String]): Unit = {

       //println(new test(1,8).el1)
       //println(memeForme(n8,Noeud(10,Noeud(25,ArbreVide,ArbreVide), ArbreVide) ))
       //println(foisn(10,n8))
       //println(n8.hauteur)
       //println(n8.contient(17))
       //println(n8.foisn(10))
       //println(n8.map({(x:Double) => x*10 }))
       //println(n8.miroir)
       //println(n8.valMax)
       //println(n8.toList(1))
       //println(Qs.transitions)
       //println(Qs.delta('a'))
       //println(auto.accepte("ccbc"))

   }
     
/* class test(x:Int, y:Int){
  require(x>0,"x doit être superieur à 0")
  def el1 = x
  def el2 = y
} */
     
trait Arbre{ 
  def hauteur:Int = this match {
      case Noeud(x, ArbreVide, ArbreVide) => 1
      case Noeud(x, fg, ArbreVide) => 1 + fg.hauteur
      case Noeud(x, ArbreVide, fd) => 1 + fd.hauteur
      case Noeud(x, fg, fd) => 1 + { if (fg.hauteur > fd.hauteur) fg.hauteur else fd.hauteur }
      case ArbreVide => 0
  }
  
  def contient[A](e:A):Boolean = this match {
      case Noeud(x, ArbreVide, ArbreVide) => e == x 
      case Noeud(x, fg, ArbreVide) => (e == x) || fg.contient(e)
      case Noeud(x, ArbreVide, fd) => (e == x) || fd.contient(e)
      case Noeud(x, fg, fd) => (e == x) || fg.contient(e) || fg.contient(e)
      case ArbreVide => false
  }
  
  def memeForme(a:Arbre):Boolean = (this,a) match {
        case (Noeud(x1, ArbreVide, ArbreVide), Noeud(x2, ArbreVide, ArbreVide)) => true
        case (Noeud(x1, fg1, ArbreVide), Noeud(x2, fg2, ArbreVide)) => fg1.memeForme(fg2)
        case (Noeud(x1, ArbreVide, fd1), Noeud(x2, ArbreVide, fd2)) => fd1.memeForme(fd2)
        case (Noeud(x1, fg1, fd1), Noeud(x2, fg2, fd2)) => fg1.memeForme(fg2) && fd1.memeForme(fd2)
        case (ArbreVide, ArbreVide) => true
        case (_ , _) => false
  }
  
  def foisn(e:Double):Arbre = this match {

    case Noeud(x:Int, ArbreVide, ArbreVide) => new Noeud(e.toInt*x, ArbreVide, ArbreVide)
    case Noeud(x:Double, ArbreVide, ArbreVide) => new Noeud(e*x, ArbreVide, ArbreVide)
    
    case Noeud(x:Int, fg, ArbreVide) => new Noeud(e.toInt*x, fg.foisn(e), ArbreVide)
    case Noeud(x:Double, fg, ArbreVide) => new Noeud(e*x, fg.foisn(e), ArbreVide)
    
    case Noeud(x:Int, ArbreVide, fd) => new Noeud(e.toInt*x, ArbreVide, fd.foisn(e))
    case Noeud(x:Double, ArbreVide, fd) => new Noeud(e*x, ArbreVide, fd.foisn(e))
    
    case Noeud(x:Int, fg, fd) => new Noeud(e.toInt*x, fg.foisn(e), fd.foisn(e))
    case Noeud(x:Double, fg, fd) => new Noeud(e*x, fg.foisn(e), fd.foisn(e))
    
    case _ => this
  }
  
  def map[A](f:Double => A):Arbre = this match {

    case Noeud(x:Int, ArbreVide, ArbreVide) => new Noeud(f(x.toDouble), ArbreVide, ArbreVide)
    case Noeud(x:Double, ArbreVide, ArbreVide) => new Noeud(f(x), ArbreVide, ArbreVide)
    
    case Noeud(x:Int, fg, ArbreVide) => new Noeud(f(x.toDouble), fg.map(f), ArbreVide)
    case Noeud(x:Double, fg, ArbreVide) => new Noeud(f(x), fg.map(f), ArbreVide)
    
    case Noeud(x:Int, ArbreVide, fd) => new Noeud(f(x.toDouble), ArbreVide, fd.map(f))
    case Noeud(x:Double, ArbreVide, fd) => new Noeud(f(x), ArbreVide, fd.map(f))
    
    case Noeud(x:Int, fg, fd) => new Noeud(f(x.toDouble), fg.map(f), fd.map(f))
    case Noeud(x:Double, fg, fd) => new Noeud(f(x), fg.map(f), fd.map(f))
    
    case _ => this
  }
  
  def miroir:Arbre = this match {
    case Noeud(x, fg, ArbreVide) => new Noeud(x, ArbreVide, fg.miroir)
    case Noeud(x, ArbreVide, fd) => new Noeud(x, fd.miroir, ArbreVide)
    case Noeud(x, fg, fd) => new Noeud(x, fd.miroir, fg.miroir)
    case _ => this
  }
  
  def valMax:Double = this match {
    case Noeud(x:Int, ArbreVide, ArbreVide) => x
    case Noeud(x:Double, ArbreVide, ArbreVide) => x
    
    case Noeud(x:Int, fg, ArbreVide) => if(x > fg.valMax) x else fg.valMax
    case Noeud(x:Double, fg, ArbreVide) => if(x > fg.valMax) x else fg.valMax
    
    case Noeud(x:Int, ArbreVide, fd) => if(x > fd.valMax) x else fd.valMax
    case Noeud(x:Double, ArbreVide, fd) => if(x > fd.valMax) x else fd.valMax
    
    case Noeud(x:Int, fg, fd) => val max_x_fg = if(x > fg.valMax) x else fg.valMax; if (max_x_fg > fd.valMax) max_x_fg else fd.valMax
    case Noeud(x:Double, fg, fd) => val max_x_fg = if(x > fg.valMax) x else fg.valMax; if (max_x_fg > fd.valMax) max_x_fg else fd.valMax
    
    case _ => error("L'arbre doit être de type Int ou Double")
  }
  
  def toList(ordre:Int):List[String] ={
    require(ordre >= -1 && ordre <=1,"l'ordre doit etre compris entre -1 et 1")
    
    if(ordre == -1){
      this match {
        case Noeud(x, fg, ArbreVide) => x.toString() :: fg.toList(ordre)
        case Noeud(x, ArbreVide, fd) => x.toString() :: fd.toList(ordre)
        case Noeud(x, fg, fd) => x.toString() :: (fg.toList(ordre) ::: fd.toList(ordre))
        case _ => Nil
      }
    }
    else if (ordre == 0){
      this match {
        case Noeud(x, fg, ArbreVide) => x.toString() :: fg.toList(ordre)
        case Noeud(x, ArbreVide, fd) => x.toString() :: fd.toList(ordre)
        case Noeud(x, fg, fd) => x.toString() :: (fd.toList(ordre) ::: fg.toList(ordre))
        case _ => Nil
      }
    }
    else {
      this match {
        case Noeud(x, fg, ArbreVide) => fg.toList(ordre) ::: List(x.toString())
        case Noeud(x, ArbreVide, fd) => fd.toList(ordre) ::: List(x.toString())
        case Noeud(x, fg, fd) => (fg.toList(ordre) ::: fd.toList(ordre)) ::: List(x.toString())
        case _ => Nil
      }
    }
  }
  
}

case object ArbreVide extends Arbre
case class Noeud[A](info:A, fg:Arbre, fd:Arbre) extends Arbre{}




}