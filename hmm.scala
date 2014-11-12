package io.celia.hmm

import scala.collection.mutable.{ArrayBuffer, Map}


case class hmm(p: Array[Double], t: Array[Array[Double]], e: Array[Map[Int, Double]]) {

	def pi: Array[Double] = p

	def T: Array[Array[Double]] = t

	def E(cat: Int)(word: Int): Double = {
		val perWordProba: Map[Int, Double] = e(cat)
		if (perWordProba.contains(word)) perWordProba(word)
		else -math.log(15)
	}

	def this(corpus: Corpus) = {
		this(corpus.probaInit(), corpus.transitionProba(), corpus.emissionProba())
	}

	def viterbi(corpus: Corpus): Array[Array[Int]] = {
		val nbCat: Int = this.pi.length

		val sentences = corpus.words
		val nbSentences = sentences.length

		val lengthSentences = new Array[Double](nbSentences)
		for (i <- 0 until nbSentences) lengthSentences(i) = sentences(i).length
		val maxLengthSentence = Algos.maxTab(lengthSentences)
		val results = Array.ofDim[Array[Int]](nbSentences)

		var indexSentence = 0

		val delta = Array.ofDim[Double](nbCat, maxLengthSentence.toInt + 1)
		val psi = Array.ofDim[Int](nbCat, maxLengthSentence.toInt + 1)
		val product: Array[Double] = new Array[Double](nbCat) //stocke les delta * t * e

		for (sentence <- sentences) {

			//Initialisation du treillis
			for (cat <- 0 until nbCat) delta(cat)(0) = pi(cat) + E(cat)(sentence(0))

			//Etape récursive
			var t = 1
			for (word <- sentence.drop(1)) {
				for (cat <- 0 until nbCat) {
					for (i <- 0 until nbCat) product(i) = delta(i)(t - 1) + T(i)(cat) + E(cat)(word)

					delta(cat)(t) = Algos.maxTab(product)

					//Stockage du meilleur état précédent
					psi(cat)(t) = Algos.argmaxTab(product)

				}
				t += 1
			}

			//Détermination du meilleur chemin
			val x = new Array[Int](t)
			t = t - 1
			x(t) = Algos.argmaxTab(product)
			for (i <- (0 until t - 1).reverse) {
				x(i) = psi(x(i + 1))(i + 1)
			}
			results(indexSentence) = x
			indexSentence += 1
		}
		results
	}

	def testViterbi(corpus: Corpus): Double = {
		val viterbiResult: Array[Array[Int]] = this.viterbi(corpus)

		val trueCategories: ArrayBuffer[ArrayBuffer[Int]] = corpus.categories

		var errorCount: Int = 0
		var wordCount: Int = 0

		for (sentenceCount <- 0 until viterbiResult.length) {
			for (((word, refCat), hypCat) <-
					 corpus.words(sentenceCount)
						 .zip(corpus.categories(sentenceCount))
						 .zip(viterbiResult(sentenceCount))) {
				val error: Boolean = hypCat != refCat
				if (error) {
					errorCount += 1
				}
				wordCount += 1
				//println(f"${if (error) "> " else "  "} $word%5d $refCat%2d $hypCat%2d")
			}
			//println()
		}
		return 1 - errorCount.toDouble / wordCount
	}


}
