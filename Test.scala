package io.celia.hmm

import scala.collection.mutable.ArrayBuffer
import scala.io.BufferedSource

object Test extends App {
	var fileName = "C:/Users/chate_000/IdeaProjects/Talep/ftb.train.encode"
	var corpus_train = new Corpus(fileName)
	var hmmTest = new hmm(corpus_train)
	var corpus_test = new Corpus("C:/Users/chate_000/IdeaProjects/Talep/ftb.dev.encode")
	//println(hmmTest.viterbi(corpus_test))
	/*for (i <- 0 until corpus_train.emissionProba().length) {
		println(corpus_train.emissionProba()(i).mkString(" "))
	}*/
	//  println(hmmTest.testViterbi(corpus_test))
	println(hmmTest.testViterbi(corpus_train))
	println(hmmTest.testViterbi(corpus_test))

	//var hmmPoney = new hmm(Array[Double](), Array[Array[Double]](), Array[Array[Double]]())
	//hmmPoney.initByCorpus(corpus_test)
	//print(hmmPoney.pi.mkString("[", ",", "]"))
}
