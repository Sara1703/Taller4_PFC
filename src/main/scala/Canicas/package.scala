package object Canicas {
    type Frasco = (Int, Int)

    type Distr = List[Frasco]

    def canicasPosiblesFrasco(f:Int, c:Int): List[Frasco] = {

        (for {i <- 0 to c } yield (f,i)).toList
    }
    
    def canicasPorFrasco(n:Int, c: Int): List[Distr] = {

        (for {frasco <- 1 to n} yield canicasPosiblesFrasco(frasco,c)).toList

    }

    def mezclarLCanicas(lc:List[Distr]): List[Distr] = lc match {

        case Nil => List(Nil)
        case head :: tail => 

            for {
                cantidadFrasco <- head
                combinar <- mezclarLCanicas(tail)
            } yield cantidadFrasco::combinar

    }
}