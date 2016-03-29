package hmm

import hmm.types._
import hmm.ourHMM._
import hmm.inference._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.OneTimeMPE
import com.cra.figaro.algorithm.factored.MPEVariableElimination

object examples {

    def VEexample() = {

        val hmm = HMM(6)
        
        println("Probability it is Sunny on day 2")
        println("Prior probability: " + VariableElimination.probability(hmm.hidden(2), Sunny))
        
        hmm.observable(2).observe(Walk)
        println("After observing Walk on day 2: " + VariableElimination.probability(hmm.hidden(2), Sunny))
        
        hmm.observable(1).observe(Walk)
        println("After observing Walk on day 1: " + VariableElimination.probability(hmm.hidden(2), Sunny))
        
        hmm.observable(0).observe(Walk)
        println("After observing Walk on day 0: " + VariableElimination.probability(hmm.hidden(2), Sunny))
        
        hmm.observable(3).observe(Walk)
        println("After observing Walk on day 3: " + VariableElimination.probability(hmm.hidden(2), Sunny))
        
        hmm.observable(4).observe(Walk)
        println("After observing Walk on day 4: " + VariableElimination.probability(hmm.hidden(2), Sunny))
        
        hmm.observable(5).observe(Walk)
        println("After observing Walk on day 5: " + VariableElimination.probability(hmm.hidden(2), Sunny))

    }

    def mostLikelyExample() = {
    
        // Make an HMM of length 4 with the parameters from Stamp's tutorial
        // Stamp <=> us conversion:
        // H = Sunny, C = Rainy
        // 0 = Clean, 1 = Shop, 2 = Walk

        val hmm = HMM(4)
        
        // Make a sequence of observations
        val observationSequence : List[O] = List(Clean, Shop, Clean, Walk)
        
        // Register these observations in our HMM
        hmm
        .observable
        .zip(observationSequence)
        .map(x => x._1.observe(x._2))
        
        // Compute mostLikelys
        val mostLikelyStates = mostLikelyHiddenStates(hmm)
        val mostLikelySequence = mostLikelyHiddenSequence(hmm)
        
        // In this example, 
        // mostLikelyHiddenStates = List(Rainy, Sunny, Rainy, Sunny)
        // mostLikelyHiddenSequence = List(Rainy, Rainy, Rainy, Sunny)
        // we have to do a little hack to reformat these to compute the probability of each

        // Reformat List[Element] as an Element[Tuple]
        val t = for {
            a <- hmm.hidden(0)
            b <- hmm.hidden(1)
            c <- hmm.hidden(2)
            d <- hmm.hidden(3)
        } yield (a,b,c,d) // : Element[(H,H,H,H)] rather than List[Element[H]]

        // ...and compute probabilities
        val p_states = VariableElimination.probability(t, (Rainy, Sunny, Rainy, Sunny))
        val p_sequence = VariableElimination.probability(t, (Rainy, Rainy, Rainy, Sunny))

        // Results
        println("For the observation sequence:")
        println(observationSequence)
        println("The most likely hidden states are:")
        println(mostLikelyStates)
        println("which has probability " + p_states)
        println("The most likely hidden sequence is:")
        println(mostLikelySequence)
        println("which has probability " + p_sequence)
    }    

}