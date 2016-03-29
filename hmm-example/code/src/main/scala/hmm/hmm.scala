package hmm

import com.cra.figaro.language._
import hmm.types._

object ourHMM {

    type H = Product with Serializable with Hidden
    type O = Product with Serializable with Observable
    
    
    def transition(rainyTransition : Array[Double], sunnyTransition : Array[Double])(e : Element[H]) : Element[H] = {
        
        def transitionGivenState(h : H) : Element[H] = {
            h match {
                case Rainy => Select(rainyTransition(0) -> Rainy, rainyTransition(1) -> Sunny)
                case Sunny => Select(sunnyTransition(0) -> Rainy, sunnyTransition(1) -> Sunny)            
            }
        }
        
        e.flatMap(transitionGivenState) 
    
    }
    
    def emission(rainyEmission : Array[Double], sunnyEmission : Array[Double])(e: Element[H]) : Element[O] = {
        
        def emissionGivenState(h : H) : Element[O] = {
            h match {
                case Rainy => Select(rainyEmission(0) -> Shop, rainyEmission(1) -> Clean, rainyEmission(2) -> Walk)
                case Sunny => Select(sunnyEmission(0) -> Shop, sunnyEmission(1) -> Clean, sunnyEmission(2) -> Walk)            
            }
        }
    
        e.flatMap(emissionGivenState)
     
    }
    
    def makeHiddenSequence(length : Int, transitionMatrix : Element[H] => Element[H], aprioriArray : Array[Double]) : List[Element[H]] = {
    
        val apriori = Select(aprioriArray(0) -> Rainy, aprioriArray(1) -> Sunny)
        
        def buildSequence(n : Int, l: List[Element[H]]) : List[Element[H]] = {
            (n, l) match {
                case (0,l) => l
                case (n, Nil) => buildSequence(n-1, List(apriori))
                case (n, x :: xs) => buildSequence(n-1, transitionMatrix(x) :: x :: xs)
            }
        }
        
        // reverse so time increases from left to right.
        buildSequence(length, Nil).reverse
    }  
    
    def makeObservableSequence(hiddenSequence : List[Element[H]], emissionMatrix : Element[H] => Element[O]) : List[Element[O]] = {
        hiddenSequence.map(emissionMatrix)
    }
    
    
    class HMM(
        val length : Int
        , val apriori : Array[Double]
        , val rainyT : Array[Double]
        , val sunnyT : Array[Double]
        , val rainyE : Array[Double]
        , val sunnyE : Array[Double]
    ) {
        val hidden : List[Element[H]] = makeHiddenSequence(length, transition(rainyT, sunnyT), apriori)
        val observable : List[Element[O]] = makeObservableSequence(hidden, emission(rainyE, sunnyE))
    }
    
    object HMM {
        val apriori = Array(0.4, 0.6)   
        val rainyT = Array(0.6, 0.4)
        val sunnyT = Array(0.3, 0.7)
        val rainyE = Array(0.2, 0.7, 0.1)
        val sunnyE = Array(0.4, 0.1, 0.5)
        def apply(length : Int) : HMM = new HMM(length, apriori, rainyT, sunnyT, rainyE, sunnyE)
        def apply(
            length: Int
            , apriori : Array[Double]
            , rainyT : Array[Double]
            , sunnyT : Array[Double]
            , rainyE : Array[Double]
            , sunnyE : Array[Double]
            ) : HMM = new HMM(length, apriori, rainyT, sunnyT, rainyE, sunnyE)
    }
}