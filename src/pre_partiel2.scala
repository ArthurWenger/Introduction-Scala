object pre_partiel2 {
  def main(args: Array[String]):Unit = {
    //println(map2(List(1,2,3),List(10,11,12),(x:Int,y:Int)=> x+y))
    println(estPrefixe(List(2,3),List(2,3,3,4)))
  }
  
  def extrait[A](a:Int, b:Int, l:List[A]):List[A] ={
    def sub1[A](a:Int, l:List[A]):List[A] ={
    if(l.isEmpty)
      l
    else
      if(a > 2) 
        sub1(a-1,l.tail)
      else
        l.tail
        
    }
    def sub2[A](b:Int, l:List[A]):List[A] ={
    if(l.isEmpty)
      l
    else
      if(b > 0) 
        l.head :: sub2(b-1,l.tail)
      else
        Nil
    }
    sub2(b,sub1(a,l))
        
  }
  
  /*def decompress[A](l:List[(Int,A)]):List[A]= {
    def sub[A](nb:Int,a:A):List[A]={
      if(nb>0)
        a::sub(nb-1,a)
       else
         Nil
    }
    l match {
    case Nil => Nil
    case (nb,a)::t => sub(nb,a) ::: decompress(t)
    }
  }*/
    
    def compress[A](l:List[A]):List[(Int,A)]= {
    def sub[A](l:List[A],acc:Int, last:A):List[(Int,A)]= l match {
      case Nil => List((acc,last))
      case h::t => if(last == h) sub(t,acc+1,last)
                   else (acc,last)::sub(t,1,h)
    }
    l match {
      case Nil => Nil
      case h::t => sub(t,1,h)
    }
    }
    
    def map2[A](l1:List[A], l2:List[A], f:(A,A)=> A):List[A] = (l1,l2) match{
      case (h1::t1, h2::t2) => f(h1,h2) :: map2(t1,t2,f)
      case (_,_) => Nil
    }
    
    def estPrefixe[A](l1:List[A],l2:List[A]):Boolean ={
      map2(l1,l2,(x:A,y:A)=>y) == l1
    } 
    
    trait Arbre{
      def info:Double
      def fg:Arbre
      def fd:Arbre
      
      def listeFeuilles():List[Arbre]= this match {
        case Noeud(_, ArbreVide, ArbreVide) => List(this)
        case Noeud(_, fg, ArbreVide) => fg.listeFeuilles()
        case Noeud(_, ArbreVide, fd) => fd.listeFeuilles()
        case Noeud(_, fg, fd) => fg.listeFeuilles() ::: fd.listeFeuilles()
        case _ => Nil
      }
      
      def sommation():Arbre={
        def sub(t:Arbre):Double = t match {
          case ArbreVide => 0
          case Noeud(v, fg, fd) => v + sub(fg) + sub(fd)
        }
        
        this match {
          case Noeud(_, fg, fd) => Noeud(sub(this),fg.sommation(), fd.sommation)
          case ArbreVide => ArbreVide 
        
      }
      }
      
      def filterA(f:Double => Boolean): List[Double] = this match {
        case ArbreVide => Nil
        case Noeud(v, fg, fd) => if(f(v)) v::(fg.filterA(f) ::: fd.filterA(f))
                                  else
                                    fg.filterA(f) ::: fd.filterA(f)
      }
    }
    
    case class Noeud(info:Double, fg:Arbre, fd:Arbre) extends Arbre
    
    case object ArbreVide extends Arbre {
      def info=sys.error("operation non definie")
      def fg=sys.error("operation non definie")
      def fd=sys.error("operation non definie")
    }
    
    val n1 = Noeud(5, Noeud(3,ArbreVide, ArbreVide), Noeud(6,Noeud(3,ArbreVide, ArbreVide), ArbreVide))
    
    
  
}