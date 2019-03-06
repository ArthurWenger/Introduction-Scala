/*
 * Devoir de Programmation d�clarative
 * Arthur Wenger 36000244
 * Lucas Dubart 35007797
 * 
 */

object Devoir {
   def main(args: Array[String]): Unit = {
     
     // L'ensemble des 30 faits qui apparaissent dans la base de r�gles
     val fait1 = Fait(1, "a des poils")
     val fait2 = Fait(2, "est un mammif�re")
     val fait3 = Fait(3, "donne du lait")
     val fait4 = Fait(4, "mange de la viande")
     val fait5 = Fait(5, "est un carnivore")
     val fait6 = Fait(6, "a des dents pointues")
     val fait7 = Fait(7, "a des griffes")
     val fait8 = Fait(8, "a les yeux vers avant")
     val fait9 = Fait(9, "a des sabots")
     val fait10 = Fait(10, "est un ongul�")
     val fait11 = Fait(11, "n'est pas un carnivore")
     val fait12 = Fait(12, "est de couleur brune")
     val fait13 = Fait(13, "a des t�ches sombres")
     val fait14 = Fait(14, "est un gu�pard")
     val fait15 = Fait(15, "a des raies noires")
     val fait16 = Fait(16, "est un tigre")
     val fait17 = Fait(17, "a un long cou")
     val fait18 = Fait(18, "a des longues pattes")
     val fait19 = Fait(19, "est une girafe")
     val fait20 = Fait(20, "est un z�bre")
     val fait21 = Fait(21, "a des plumes")
     val fait22 = Fait(22, "est un oiseau")
     val fait23 = Fait(23, "vole")
     val fait24 = Fait(24, "ponds des oeufs")
     val fait25 = Fait(25, "ne vole pas")
     val fait26 = Fait(26, "est noir et blanc")
     val fait27 = Fait(27, "est une autruche")
     val fait28 = Fait(28, "nage")
     val fait29 = Fait(29, "est un pingouin")
     val fait30 = Fait(30, "est un aigle")
     
     // L'ensemble des 15 r�gles de l'�nonc�
     val regle1 = Regle(1, List(fait1), List(fait2))
     val regle2 = Regle(2, List(fait3), List(fait2))
     val regle3 = Regle(3, List(fait4), List(fait5))
     val regle4 = Regle(4, List(fait6,fait7,fait8), List(fait5))
     val regle5 = Regle(5, List(fait2, fait9), List(fait10))
     val regle6 = Regle(6, List(fait2, fait11), List(fait10))
     val regle7 = Regle(7, List(fait2, fait5, fait12, fait13), List(fait14))
     val regle8 = Regle(8, List(fait2, fait5, fait12, fait15), List(fait16))
     val regle9 = Regle(9, List(fait10, fait17, fait18, fait13), List(fait19))
     val regle10 = Regle(10, List(fait10, fait15), List(fait20))
     val regle11 = Regle(11, List(fait21), List(fait22))
     val regle12 = Regle(12, List(fait23, fait24), List(fait22))
     val regle13 = Regle(13, List(fait22, fait25, fait17, fait18, fait26), List(fait27))
     val regle14 = Regle(14, List(fait22, fait25, fait28, fait26), List(fait29))
     val regle15 = Regle(15, List(fait22, fait5), List(fait30))

     // La base de r�gles
     val bdr = List(regle1, regle2, regle3, regle4, regle5, regle6, regle7, regle8, regle9, regle10, regle11, regle12, regle13, regle14, regle15)
     // La base de faits initiale
     val bdf_init = List(fait3, fait4, fait6, fait12, fait15)
     // Le fait que l'on souhaite demontrer
     val hyp = fait14
     
     // Affichage de la base de faits et de la base de r�gles
     afficheBase(bdr, "*** Base de Faits initiale ***")
     afficheBase(bdf_init, "*** Base de r�gles ***")
     
     println("******************************************************************")
     println("===> lancement du chainage avant pour d�montrer : " + hyp.intitule +"\n")
     val (bdf_fwd, regles_util_fwd) = chainageAvant(bdr, bdf_init)
     // Affichage des nouveaux faits obtenus
     afficheChangements(bdf_init, bdf_fwd, regles_util_fwd)
     
     if( bdf_fwd.contains(hyp) ) println("\n==> R�sultat : " + hyp.intitule + " est d�montr�")
     else                        println("\n==> R�sultat : " + hyp.intitule + " n�est pas d�montrable")

     println("\n******************************************************************")
     println("===> lancement du chainage arri�re pour d�montrer : " + hyp.intitule +"\n")
     
     val (_, bdf_bwd, regles_util_bwd) = chainageArriere(bdr, bdf_init, hyp, Nil)
     // Affichage des nouveaux faits obtenus
     afficheChangements(bdf_init, bdf_bwd, regles_util_bwd)
     
     if( bdf_bwd.contains(hyp) ) println("\n==> R�sultat : " + hyp.intitule + " est d�montr�")
     else                        println("\n==> R�sultat : " + hyp.intitule + " n�est pas d�montrable")
   }
   
   case class Fait(id:Int, intitule:String){
     override def toString() ={ intitule }
   }
   
   case class Regle(id:Int, hypothese: List[Fait], conclusion: List[Fait]){
     override def toString() ={
       def insert_ET(l:List[Fait]): String = l match {
         case Nil    => ""
         case h::Nil => h.intitule
         case h::t   =>  h.intitule + " ET " + insert_ET(t)  
         }
       "SI " + insert_ET(hypothese) + " ALORS " + insert_ET(conclusion)
       }
     }
   
  /* Fonction permettant de g�n�raliser les fonctions afficheBDFaits et afficheBDRegles de l'�nonc�.
   * Entr�es: une liste d'�lements A (bd) et un message d'introduction sous la forme d'une cha�ne de caract�res (intro_message).
   * Sortie: le message d'introduction suivi de l'affichage de chacun des �lements de la liste bd. */
  def afficheBase[A](bd:List[A], intro_message:String):Unit ={
      println(intro_message +"\n")
      bd.map(println)
      println()
  }
  
  /* Fonction qui permet d'afficher les nouveaux faits obtenus apr�s l'�x�cution d'un chainage avant / arriere.
   * Entr�es: - la base de fait initiaux (bdf_init)
   *          - la base de faits apr�s l'�x�cution du chainage (bdf_update) 
   *          - la liste des regles utilis�es pendant le chainage (reglesUtil)
   * Sortie: Affichage de la difference entre la base de faits avant et apr�s le chainage. */
  def afficheChangements (bdf_init: List[Fait], bdf_update: List[Fait], reglesUtil: List[Regle]):Unit ={
    if(reglesUtil == Nil) println("\nAucun nouveau fait obtenu\n")
    else { println("\n+++ Les nouveaux faits sont:\n"); (bdf_update diff bdf_init).map(println) }
           // On aurait aussi pu utiliser: reglesUtil.map({(r:Regle) => println(r.conclusion)})                         
  }
  
  /* Fonction qui teste si une r�gle est applicable. Une r�gle est applicable si elle n�a pas d�j� �t� utilis�e
   * et si tous les faits de son hypoth�se sont dans la base de faits connus. 
   * Entr�es: - la r�gle � tester (r) 
   * 					- la liste des r�gles utilis�es (reglesUtil) 
   * 					- la base de faits connus (bdfConnus)
   * Sortie: vrai si la r�gle est applicable et faux sinon. */
  def estApplicable(r: Regle, reglesUtil: List[Regle], bdfConnus: List[Fait]): Boolean ={
     !reglesUtil.contains(r) && r.hypothese.forall(bdfConnus.contains)
  }
  
  /* Fonction qui applique la r�gle donn�e en param�tre c�est � dire retourne comme r�sultat la liste des faits connus
	 * dans laquelle on �t� ajout� tous les faits de la conclusion de la r�gle et la liste des r�gles utilis�es dans
	 * laquelle on a ajout� la r�gle appliqu�e.
   * Entr�es: - la r�gle � appliquer (r)
   * 					- la base des faits connus (bdfConnus)
   * 					- la liste des r�gles utilis�es (reglesUtil)
   * Sortie: un couple form� de la nouvelle base des faits et de la nouvelle liste des r�gles utilis�es qui ont �t� mis � jour. */
  def appliqueRegle(r: Regle, bdfConnus: List[Fait], reglesUtil: List[Regle]): (List[Fait], List[Regle]) ={
      ((bdfConnus union r.conclusion).distinct, r :: reglesUtil )
  }
  
  /* Fonction correspondant au chainage avant.
   * Entr�e : la base de r�gles (bdr) et une base de faits connus (bdfConnus)
   * Sortie : la base de faits terminale, c�est � dire bdfConnus dans laquelle on a ajout� tous les nouveaux faits qui ont �t�
	 * d�duits au cours du raisonnement, et la liste des r�gles qui on �t� utilis�es au cours le raisonnement. */
  def chainageAvant(bdr : List[Regle], bdfConnus: List[Fait]): (List[Fait], List[Regle]) ={
     def sub(bdr_iter : List[Regle], bdf_update: List[Fait], reglesUtil: List[Regle]):(List[Fait], List[Regle]) = bdr_iter match {
       case Nil => (bdf_update, reglesUtil)
       case h::t => { if(!estApplicable(bdr_iter.head, reglesUtil, bdf_update)) sub(t, bdf_update, reglesUtil)
                      else { println("<-- Application de la r�gle : " + h)
                             val (new_bdf, new_reglesUtil) = appliqueRegle(h, bdf_update, reglesUtil)
                             // La base de faits ayant �t� modifi�e, on �x�cute sub en retournant au debut de la base de regles
                             sub(bdr, new_bdf, new_reglesUtil) }}
     } 
     sub(bdr, bdfConnus, List())
  }
  
  /* Fonction permettant de r�cup�rer la liste des r�gles dont la conclusion correspond � un fait donn� en param�tre.
   *  Entr�e: la base de r�gle (bdr) et un fait (unFait)
   *  Sortie: La liste des r�gle ayant unFait dans leur conclusion. */ 
  def get_rules_from_conclusion(bdr : List[Regle], unFait: Fait) ={
    bdr.foldLeft(Nil:List[Regle])((r, c) => if(c.conclusion.contains(unFait)) (c :: r) else r)
  }
  
  /* Fonction correspondant au chainage arri�re.
   * Entr�e : - une base de r�gles (bdr)
   *  				- une base de faits connus (bdfConnus)
   *  				- le fait � d�montrer (unFait)
   *  			  - la liste des r�gles utilis�es (reglesUtil)
   * Sortie : vrai si unFait est d�montrable, faux sinon, la base de faits connus dans laquelle ont �t� ajout�s tous les nouveaux faits
	 * ainsi que la liste des r�gles utilis�es pendant le raisonnement. */
  def chainageArriere(bdr : List[Regle], bdfi : List[Fait], unFait: Fait, reglesUtil:List[Regle]) : (Boolean, List[Fait], List[Regle]) ={
    if(bdfi.contains(unFait)){
       println("<-- " + unFait.intitule + " est dans la base des faits connus")
      (true, bdfi, reglesUtil)
    }
    else {
      val list_match_rules = get_rules_from_conclusion(bdr, unFait)
      val (bool, lsF, lsR) = fromRules(bdr, bdfi, list_match_rules, reglesUtil)
      bool match { case true  => println("(+) " + unFait + " est d�montr�"); (true, unFait :: lsF, lsR)
                   case false => println("(x) " + unFait + " n'est pas d�montrable"); (false, lsF, lsR)
      }
    }
  }
  
  /* Fonction permettant de d�terminer si au moins une r�gle d'une liste est applicable apr�s �x�cution du chainage arriere sur l'ensemble des hypoth�ses
   * Entr�e : identique � la fonction chainageArriere � l'exception du param�tre unFait qui est remplac� par une liste de r�gles (desRegles)
   * Sortie : identique � la fonction chainageArriere */
  def fromRules (bdr : List[Regle], bdfi : List[Fait], desRegles: List[Regle], reglesUtil:List[Regle]): (Boolean, List[Fait], List[Regle]) = desRegles match {
      case Nil => (false, bdfi, reglesUtil)
      case h::t => { val (bool, lsF, lsR) = fromFacts(bdr, bdfi, h.hypothese, reglesUtil)
                     bool match { case true  =>  println("<-- Application de la r�gle: "+ h); (true, lsF, h :: lsR)
                                  case false =>  fromRules(bdr, lsF, t, lsR) }}
  }
   
  /* Fonction permettant de d�terminer si l'ensemble des faits d'une liste peuvent �tre d�duit apr�s �x�cution du chainage arriere sur chaque fait.
   * Entr�e: identique � la fonction chainageArriere � l'exception du param�tre unFait qui est remplac� par une liste de faits (desFaits)
   * Sortie : identique � la fonction chainageArriere */
  def fromFacts(bdr : List[Regle], bdfi : List[Fait], desFaits: List[Fait], reglesUtil:List[Regle]): (Boolean, List[Fait], List[Regle]) = desFaits match {
      case Nil  => (true, bdfi, reglesUtil)
      case h::t => { println("==> Je cherche � d�montrer: "+ h); val (bool, lsF, lsR) = chainageArriere(bdr, bdfi, h, reglesUtil)
                     bool match { case true  => fromFacts(bdr, lsF, t, lsR)
                                  case false => (false, lsF, lsR) }}
  }    
}