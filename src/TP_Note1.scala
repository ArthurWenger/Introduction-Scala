

object TP_Note1 {
  
      def main(args: Array[String]): Unit = {
      //println( has_enough_parenthesis("(bidule))"))
     //println( countChange(4,List(1,2)))
     //println( twist(List((1,2),(3,4))) )
     //println( lengthList(List(1,2,3,4)) )
     //println( concat(List(1,2,3),List(4,5,6)) )
     //println(flat(List(List(1,2,3),List(1,2,3),List(1,2,3))))
     //println(gardetantque(List(2,3,4,5),{(x:Int)=> x%2==0}))
     //println(termexp(2,3))
     //println(somme1(2,3))
     println(flip(List(1,2,3,4,5,8,2,4)))
     
     //println(somme2(1,10))
     
   }
  
  
  def test28 (acc:Int,n:Int,iter:Int):Int={
  println(acc+" n:"+n+" iter:"+iter)
  if(n>1)
    test28(acc*(iter),n-1,iter+1)
  else
    acc
}
  
def somme2(x:Int, n:Int):Double ={
  
  def sub(xp:Double, np:Double, acc:Double, iter:Double):Double ={
    if(iter<=n){ println("x:"+xp+" n:"+np)
      sub(xp*x,np*(iter+1),acc+(xp/np),iter+1)
    }
    else
      acc
  }
  
  sub(x,1,0,1)
} 

def flip(l:List[Int]):List[Int]={
  if(!l.tail.isEmpty){
    if(!l.tail.tail.isEmpty)
      l.tail.head :: (l.head :: flip(l.tail.tail))
     else
        List(l.tail.head, l.head)
  }
  else
    l
} 
  
}