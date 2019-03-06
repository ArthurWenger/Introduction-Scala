

object TD2 {
    def main(args: List[String]): Unit = {

        //println (saufDernier(List(1,2,3,4,5)))
        //println (estDans(3,List(1,2,3,4,5)))
        //println (concat(List(1,2), (List(3,4))))
        //println (tousDiff(List(1,2,3)))
        //println (saufImpair(List(1,2,3,4,5,6)))
        //println (List(1,2,3,4,5,6).head)
        //println (pairImpair(List(1,2,3,4,5,6)))
        //println (tousFois2(List(1,2,3,4,5,6)))
        //println (tousplus1bis(List(1,2,3,4,5,6)))
        //println (replace1by0(List(-1,2,-3,4,-5,6)))
        //println (splitListCouple(List((1,2),(3,4),(5,6))))
        //println (combineLists(List(1,3,5,7),List(2,4,6,8)))
        //println (supn(2,List(1,2,3,4,5,6)))
        //println (estprefixe(List(1,2,3,4,5,6), List(1,2,3)))
        //println (substitute(List(1,2,3,4,2,3,5,2,3,6,7), List(2,3), List(0)))
        //println( partage(List(1,2,3,4), { (x:Int) => (x%2 == 0) }) )
        //println( splitStringHibou(List("hello","hicou","hiaou")) )
        //println( estEnsemble(List(1,2,3,4,2)) )
        //println( union(List(1,2,3),List(2,3,4)) )
        //println( inter(List(1,2,3),List(2,3,4)) )
        //println( sauf(List(1,2,3,4,3,5),3) )
         println( enleve(List(1,2,3,4,3,5),List(3,2) ))
      }
  
    
    // Ex 2.1
    def saufDernier (l:List[Int]):List[Int] ={
      if (l.tail.tail.isEmpty)
        List(l.head)
      else
        l.head :: saufDernier(l.tail)
    }
    
    // Ex 2.2
    def estDans[A](n:A, l:List[A]):Boolean ={
      if (l.isEmpty)
        false
      else if(l.head == n)
        true
      else
        estDans(n, l.tail)
    }
    
    // Ex 2.3
    def concat[A](l1:List[A], l2:List[A]): List[A] ={
      if(!l1.isEmpty)
        l1.head :: concat (l1.tail,l2)
      else
        l2
    }
    
    // Ex 2.4
    def tousDiff(l:List[Int]):Boolean ={
      if(l.isEmpty)
        true
      else if (estDans(l.head, l.tail))
        false
      else
        tousDiff(l.tail)
    }
    
   // Ex 2.5
    def saufImpair(l:List[Int]):List[Int] ={
      if(!l.isEmpty){
        if(l.head % 2 == 1)
          saufImpair(l.tail)
        else
          l.head :: saufImpair(l.tail)
      }
      else
        List()
    }
    
    // Ex 2.6
    def pairImpair(l:List[Int]):(List[Int],List[Int])={
      
      def sub(l:List[Int], lpair:List[Int], limpair:List[Int]):(List[Int],List[Int])={
      
        if(!l.isEmpty){
          if(l.head % 2 == 1)
            sub(l.tail, lpair, l.head :: limpair)
          else
            sub(l.tail, l.head :: lpair, limpair)
        }
        else
          (lpair, limpair)
        }
        
      sub(l, List(),List())
    }
    
    // Ex 2.7
    // 1.
    def tousFois2 (l:List[Int]): List[Int] ={
      if(!l.isEmpty){
        (l.head * 2) :: tousFois2(l.tail)
      }
      else
        List()
    }
    
    // 2.
    def tousPlus1 (l:List[Int]): List[Int] ={
      if(!l.isEmpty){
        (l.head + 2) :: tousPlus1(l.tail)
      }
      else
        List()
    }
    
    // 3.
    def appliqueATous (f: Int => Int): List[Int] => List[Int] ={
      
      def sub(l:List[Int]): List[Int] ={
        if(!l.isEmpty){
          f(l.head) :: sub(l.tail)
        }
        else
          List()
        }
      
      {(l:List[Int]) => sub(l)}
    }
    
    def tousplus1bis = appliqueATous( {(x:Int) => x+1} )
    
    // 4.
    def replace1by0 = appliqueATous( {(x:Int) => if(x<0) 0 else x} )
    
    // ex 2.8
    
    def splitLeft[A](x:(A,A)): A ={
      x._1
    }
    
    def splitRight[A](x:(A,A)): A ={
      x._2
    }
    
    def splitListCouple[A] (l:List[(A,A)]) : (List[A], List[A]) ={    
      
      def sub (l1:List[A], l2:List[A], l:List[(A,A)] ): (List[A], List[A]) ={
        
      if(!l.isEmpty){
          sub( splitLeft(l.head) :: l1, splitRight(l.head) :: l2, l.tail)
        }
        else
          (l1,l2)
      }
      sub(List(),List(),l)
    }
    
    // ex 2.9
    
    def combineLists[A] (l1:List[A], l2:List[A]): List[(A,A)] ={
      if(!l1.isEmpty){
        if(!l2.isEmpty)
          (l1.head, l2.head) :: combineLists(l1.tail,l2.tail)
        else
          List()
      }
      
      else
        List()
    }
     
    // ex 2.10
    
    // 1. 
    def supn[A] (n:Int, l:List[A]): List[A] ={
      if(n>0){
        if(!l.isEmpty)
          supn(n-1,l.tail)
        else
          List()
      }
      
      else
        l
    }
      
    // 2.
    def estprefixe[A] (l:List[A],p:List[A]): Boolean ={
      if(!p.isEmpty){
        if(!l.isEmpty){
          if(p.head == l.head)
            estprefixe(p.tail, l.tail)
          else
            false
        }
        else
          false
      }
      else
        true      
    }
        
    // 3.
    def substitute[A] (l:List[A], p:List[A], s:List[A]): List[A] ={
      if(!l.isEmpty){
        if(estprefixe(l,p)){
          concat (s, substitute(supn(p.length, l), p, s))
        }
        else {
          l.head :: substitute(l.tail, p, s)
        }
      }
      else
        l
    }
    
    // 2.11
    // 1.
    def partage[A](l:List[A], f:A => Boolean):(List[A],List[A]) ={
       
      def sub (l:List[A], l1:List[A], l2:List[A]): (List[A], List[A]) ={
      if(!l.isEmpty){
        if(f(l.head))
          sub(l.tail, l.head :: l1, l2)
        else
          sub(l.tail, l1, l.head :: l2)
      }
      else {
        (l1,l2)
      }
      }
      
      sub(l, List(),List())
    }
    
    
    
    // 2.
    def pair= { (x:Int) => x%2 == 0 }
    def splitOdd = { (l:List[Int]) => partage(l,pair) }
    /* Idem que:
     def splitOdd (l:List[Int]):(List[Int],List[Int]) ={
      partage(l,pair)
    }
    */
    
    // 3.
    def hibou = { (s:String) => if(s<"hibou") true 
                                else false }
    def splitStringHibou = { (l:List[String]) => partage(l,hibou) }
    
    // ex 2.12
    
    // 1.
    
    def estEnsemble[A](l:List[A]):Boolean ={
      if(!l.isEmpty){
        if(estDans(l.head, l.tail))
          false
        else
          estEnsemble(l.tail)
      }
      else
        true
    }
    
    // 2.
    
    def union[A](l1:List[A], l2:List[A]):List[A] ={
      if(!l1.isEmpty){
          if(estDans(l1.head,l2))
              union(l1.tail,l2)
          else
            l1.head :: union(l1.tail,l2)
      }
      else
        l2
    }
    
    // 3.
    
    def inter[A](l1:List[A], l2:List[A]):List[A] ={
      if(!l1.isEmpty){
          if(estDans(l1.head,l2))
            l1.head :: inter(l1.tail,l2)
          else
            inter(l1.tail,l2)
          
      }
      else
        List()
    }
    
    // ex 2.13
    
    // 1.
    def sauf[A](l:List[A], x:A):List[A] ={
      if(!l.isEmpty){
        if(l.head == x)
          sauf(l.tail,x)
        else
          l.head :: sauf(l.tail,x)
      }
      else
        List()
    }

    // 2.
    def enleve[A](l1:List[A], l2:List[A]):List[A] ={
      if(!l2.isEmpty)
        enleve(sauf(l1,l2.head),l2.tail)
      else
        l1
    }
    
    // Exo sup.
    // Soit un montant x donné, combien d'arrangements de pièces permettent d'arriver à ce montant avec une liste de pièces définies
    // Ex (2,1,0.5)
    // On suppose les elements de la liste triés par ordre decroissant (ou sinon definir une fonction de tri)
    
    
    def getmax(x:Double,y:Double):Int ={
      if(x-y>0)
        1+getmax(x-y,y)
      else
        0 
    }  
    
    /* def piece(x:Double, l:List[Double]):Double ={
      
      def sub(x:Double, l:List[Double], max:Int, elem:Double, montant:Double):Int ={
      
      if(max>0){
        if(montant > x)
          sub(x,l,max-1,elem,montant-elem)
        else if(montant < x){
           if(!l.isEmpty)
              sub(x,l.tail,getmax(x,l.head),l.head,montant)
           else
              sub(x,l.tail,max,l.head,montant+elem)
        }
        else
          1 + sub(x,l.tail,getmax(x,l.head),l.head,montant)
        
      }
      else
        0
        
      }
      sub(x,l,getmax(l.head), 0)
    } */
    
    /*
     *       def sub(x:Double, l:List[Double], acc:Double):Boolean ={
        if(l.isEmpty)
          false
        else
          if((l.head + acc) > x  )
            sub(x,l.tail,acc)
          else { 
            if((l.head + acc) == x)
              true
            else
             sub(x,l,acc+l.head)
          } 
      }
     * 
     * 
     */
    
}  


   