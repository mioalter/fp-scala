package hmm

object types {

    sealed trait Hidden
    case object Rainy extends Hidden
    case object Sunny extends Hidden
    
    sealed trait Observable
    case object Shop extends Observable
    case object Clean extends Observable
    case object Walk extends Observable


}