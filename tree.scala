sealed trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

sealed trait Tree[+A] {

    /**
    * Méthode pour connaître la taille de l'arbre
    * +1 à chaque branche et chaque feuille
    */
    def size: Int = this match {
        case Leaf(_) => 1
        case Branch(l,r) => 1 + l.size + r.size 
    }

    /**	
    * Méthode pour connaître le nombre de feuilles de l'arbre
    * +1 à chaque feuille
    */
    def count: Int = this match {
        case Leaf(_) => 1
        case Branch(l,r) => l.count + r.count 
    }

    /**	
    * Méthode pour connaître le maximum de l'arbre
    * parcours récursif de l'arbre pour avoir les valeurs de chaque feuille
    * comparaison avec f
    */
    def max(f: (A, A) => Boolean): A = this match {
        case Leaf(v) => v
        case Branch(l, r) => if(f(l max f, r max f)) r max f else l max f
    }

    def find(f: A => Boolean): Option[A] = this match {
        case Leaf(v) if (f(v)) => Some(v)
        case Branch(l, r) => l.find(f).orElse(r.find(f))
        case _ => None
    }

    def depth: Int = this match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + l.depth max r.depth
    }

    def map[B](f: A => B): Tree[B] = this match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(l.map(f), r.map(f))
    }

    def fold[B](lf: A => B, bf: (B, B) => B): B = this match {
    	case Leaf(v) => lf(v)
    	case Branch(l, r) => bf(l.fold(lf, bf), r.fold(lf, bf))
    }

    def size2: Int = {
    	fold(_ => 1, (a: Int, b: Int) => 1 + a + b)
    }

    def count2: Int = {
    	fold(_ => 1, (a: Int, b: Int) => a + b)
    }
    
    def max2(f: (A, A) => Boolean): A = {
    	fold((a: A) => a, (l: A, r: A) => if (f(l, r)) r else l)
    }

    def depth2: Int = {
    	fold(_ => 1, (l: Int, r: Int) => 1 + l max r)
    }

    def map2[B](f: A => B): Tree[B] = {
    	fold((a:A) => Leaf(f(a)), (l: Tree[B], r: Tree[B]) => Branch(l, r))
    }

    def toList: List[A] = {
    	fold((a: A) => List(a), (l: List[A], r: List[A]) => l:::r)
    }

    def rand(rng: RNG): A = this match {
        case Leaf(v) => v
        case Branch(l, r) => {
             val (choice, newRNG) = rng.nextInt
             if(choice % 2 == 0) l.rand(newRNG) else r.rand(newRNG)
        }
    }
}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def max(t: Tree[Int]): Int = t match {
        case Leaf(v) => v
        case Branch(l, r) => max(l).max(max(r))
    }
}

object TestTree {

    import Tree._

    def main(args: Array[String]): Unit = {

        /* Impossible de faire un arbre vide car pas de gestion de none (option)
        * Leaf hérite de Tree donc c'est possible de faire un arbre avec une seule feuille (b)
        * Idem pour les arbres à nbre de feuilles quelconques, il suffira de bien manipuler Leaf et Branch selon la topologie voulue
        */
        /*val void = Branch()*/
        val single = Leaf(42)
        val random = Branch(Branch(Leaf(2), Leaf(8)), Branch(Leaf(13), Leaf(4)))

        println(single)
        println(random)

        val a = Branch(Branch(Branch(Leaf(4), Leaf(5)), Leaf(8)), Branch(Leaf(13), Leaf(9)))
        val b = Branch(Branch(Branch(Leaf("c"), Leaf("p")), Leaf("x")), Branch(Leaf("q"), Leaf("a")))

        println()
        println(a)
        println("Taille de a = " + a.size)
        println("Nombre de feuilles de a = " + a.count)
        println("Max de a = " + max(a))
        println("Max de a avec méthode = " + a.max((a: Int,b: Int) => a < b))
        println()
        println(b)
        println("Max de b = " + b.max((a: String,b: String) => a < b))
        println()
        
        println("Cherchons 8 dans a :")
        val sa = a.find(v => v == 8)
        sa match {
           case Some(v) => println(s"La valeur $v est bien dans a !")
           case None => println("Cette valeur n'est pas dans a !")
        }
        println()

        println("Cherchons o dans b :")
        val sb = b.find(v => v == "o")
        sb match {
           case Some(v) => println(s"La valeur $v est bien dans b !")
           case None => println("Cette valeur n'est pas dans b !")
        }
        println()

        println("Profondeur de a = " + a.depth)
        println()
        val c = a.map(_+1)
        println("Map de a : " + c)
        val d = b.map(_+"a")
        println("Map de b : " + d)
        println()
        println("Taille de a avec size2 : " + a.size2)
        println("Nombre de feuilles de a avec count2 : " + a.count2)
        println("Max de a avec max2 : " + a.max2((a: Int,b: Int) => a < b))
        println("Max de b avec max2 : " + b.max2((a: String,b: String) => a < b))
        println("Profondeur de a avec depth2 : " + a.depth2)
        val e = a.map2(_+1)
        println("Map2 de a : " + e)
        val f = b.map2(_+"a")
        println("Map2 de b : " + f)
        println()
        println("a sous forme de liste : " + a.toList)
        println()
        val rng = SimpleRNG(54)
        println("Valeur d'une feuille aléatoire de a : " + a.rand(rng))
        val rng2 = SimpleRNG(108)
        println("Valeur d'une autre feuille aléatoire de a : " + a.rand(rng2))
    }
}