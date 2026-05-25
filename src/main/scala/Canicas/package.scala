import Canicas.Distr

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


    //todo  'M' canicas se tienen que distribuir en 'N' frascos y cada frasco tiene una capacidad C
    //TODO Calcular todas las  formas de distribuir en los frascos respetando la capacidad de fracaso

    def  distribucion(m : Int, n: Int, c:Int): List [Distr] = {
        val opcionesPorFrasco : List[Distr] =
            for {
            frasco <- (1 to n) .toList
        } yield (0 to c).toList.map(cant => (frasco,cant))

        val combinacionesFrasco: List[Distr] = mezclarLCanicas(opcionesPorFrasco)

        for  {distribucion <- combinacionesFrasco
              if  distribucion.map(_._2).sum == m
              }yield  distribucion
    }
}

def agrupaciones(m: Int): List[List[Int]] = {

  def subConjunto(r: Int, m:Int) : List[List[Int]] = {

      if (r == 0) List(List())
      else {
          for {
              agrupacion <- (m to r).toList
              resto <- subConjunto(r - agrupacion, agrupacion + 1)
          } yield agrupacion :: resto
      }
  }
subConjunto(m,1)
}




