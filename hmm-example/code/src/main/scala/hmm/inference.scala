package hmm

import hmm.types._
import hmm.ourHMM._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.OneTimeMPE
import com.cra.figaro.algorithm.factored.MPEVariableElimination

object inference {
        
    def makeSample(obs : List[Element[O]]) : List[O] = {
        
        def toObs(a : Any) : O = {
            a match {
                case Clean => Clean
                case Shop => Shop
                case Walk => Walk
            }
        }
    
        def makeObservation(node: Element[O]) : O = {
            val alg = Importance(1, node)
            val observation = toObs(alg.sample()._2.head._2)
            node.observe(observation)
            observation
        }

        obs.map(makeObservation)
    }

    def generateSamples(length : Int, numSamples : Int) : List[List[O]] = {
        val hmms = List.fill(numSamples)(HMM(length))
        hmms.map(x => makeSample(x.observable))
    }    


    def mostLikelyValue[A](e : Element[A], vals : List[A]) : A = {
        vals
        .map(x => (x, VariableElimination.probability(e, x)))
        .sortBy(x => x._2)
        .reverse
        .head
        ._1
    }
    
    def mostLikelyHiddenStates(hmm : HMM) : List[H] = {
        
        def mostLikelyState(e : Element[H]) : H = {
            mostLikelyValue[H](e, List(Rainy, Sunny))
        }
    
        hmm.hidden.map(x => mostLikelyState(x))

    }
    
    def mostLikelyHiddenSequence(hmm : HMM) : List[H] = {
            
        def run(algo : OneTimeMPE, hmm : HMM) : List[H] = {
            algo.start()
            val likelyHidden = hmm.hidden.map(x => algo.mostLikelyValue(x))
            algo.kill
            likelyHidden
        }
        
        run(MPEVariableElimination(), hmm)

    }

}    