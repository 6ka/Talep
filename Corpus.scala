package io.celia.hmm

import scala.collection.mutable.{ArrayBuffer, Map}
import scala.io.BufferedSource


class Corpus(w: ArrayBuffer[ArrayBuffer[Int]], c: ArrayBuffer[ArrayBuffer[Int]]) {

	def words: ArrayBuffer[ArrayBuffer[Int]] = w

	def categories: ArrayBuffer[ArrayBuffer[Int]] = c

	def this(name: String) = {
		this(Algos.wordsByFile(name), Algos.categoriesByFile(name))
	}

	def nbSequences(): Int = {
		return this.categories.length
	}

	val MIN = -1.0e307


	val (initCat, unigramCat, bigramCat, wordCategories): (Map[Int, Int], Map[Int, Int], Map[(Int, Int), Int], Map[Int, Map[Int, Int]]) = this.build()
	val nbCat = unigramCat.size

	def build(): (Map[Int, Int], Map[Int, Int], Map[(Int, Int), Int], Map[Int, Map[Int, Int]]) = {
		var initCat = Map[Int, Int]()
		var wordCategories = Map[Int, Map[Int, Int]]()
		var unigramCategories: Map[Int, Int] = Map[Int, Int]()
		var bigramCategories: Map[(Int, Int), Int] = Map[(Int, Int), Int]()

		for (i <- 0 until this.nbSequences()) {
			val initCurrentCat: Int = this.categories(i)(0)
			for (j <- 0 until this.words(i).length) {
				val currentWord = this.words(i)(j)
				val currentCat = this.categories(i)(j)
				if (wordCategories.contains(currentWord)) {
					if (wordCategories(currentWord).contains(currentCat)) {
						wordCategories(currentWord)(currentCat) += 1
					}
					else {
						wordCategories(currentWord) += (currentCat -> 1)
					}
				}
				else {
					wordCategories += (currentWord -> Map(currentCat -> 1))
				}
			}

			for (j <- 0 until this.categories(i).length - 1) {
				var currentCat = this.categories(i)(j)
				var nextCat = this.categories(i)(j + 1)
				if (unigramCategories.contains(currentCat)) {
					unigramCategories(currentCat) += 1
					if (bigramCategories.contains((currentCat, nextCat))) {
						bigramCategories(currentCat, nextCat) += 1
					}
					else {
						bigramCategories += ((currentCat, nextCat) -> 1)
					}
				}
				else {
					unigramCategories += (currentCat -> 1)
					bigramCategories += ((currentCat, nextCat) -> 1)
				}
			}

			if (initCat.contains(initCurrentCat)) {
				initCat(initCurrentCat) += 1
			}
			else {
				initCat += (initCurrentCat -> 1)
			}
		}
		return (initCat, unigramCategories, bigramCategories, wordCategories)
	}

	def probaInit(): Array[Double] = {
		val p = new Array[Double](nbCat)
		for ((cat, initialCount) <- initCat) {
			if (initialCount != 0)
				p(cat) = math.log(initialCount) - math.log(nbSequences())
			else p(cat) = MIN
		}
		p
	}

	def transitionProba(): Array[Array[Double]] = {
		val t = Array.ofDim[Double](nbCat, nbCat)

		for ((cat1, count1) <- unigramCat) {
			for ((cat2, count2) <- unigramCat) {
				if (bigramCat.contains((cat1, cat2)))
					t(cat1)(cat2) = math.log(bigramCat(cat1, cat2).toDouble) - math.log(unigramCat(cat1))
				else
					t(cat1)(cat2) = MIN
			}
		}
		return t
	}

	def emissionProba(): Array[Map[Int, Double]] = {
		val words = wordCategories.keys
		val wordList = words.toList
		val nbWords = words.size
		//val e = Array.ofDim[Double](nbCat, nbWords)
		val e = new Array[Map[Int, Double]](nbCat)

		for (i <- 0 until nbCat)
			e(i) = Map[Int, Double]()

		for ((cat, count) <- unigramCat) {
			for (word <- wordList) {
				var proba: Double = MIN
				if (wordCategories.contains(word)) {
					if (wordCategories(word).contains(cat))
						proba = math.log(wordCategories(word)(cat)) - math.log(unigramCat(cat))
				}
				else {
					proba = -math.log(nbCat)
				}

				e(cat) += (word -> proba)
			}
		}
		return e
	}


}
