package io.celia.hmm

import scala.collection.mutable.ArrayBuffer
import scala.io.BufferedSource

/**
 * Created by chate_000 on 23/10/2014.
 */
object Algos {

  def wordsByFile(name : String): ArrayBuffer[ArrayBuffer[Int]] ={
    val train_file: BufferedSource = scala.io.Source.fromFile(name)

    val train_lines = train_file.getLines()
    var current_words = new ArrayBuffer[Int]()
    val words = new ArrayBuffer[ArrayBuffer[Int]]()

    train_lines.foreach{
      s =>
        if (!s.isEmpty()) {
          val line = s.split(" ")
          current_words += line(0).toInt
        }
        else{
          words.append(current_words)
          current_words = new ArrayBuffer[Int]()
        }
    }
    return words
    //return corpus
  }

  def categoriesByFile(name : String) : ArrayBuffer[ArrayBuffer[Int]] = {
    val train_file: BufferedSource = scala.io.Source.fromFile(name)

    val train_lines = train_file.getLines()
    var current_categories = new ArrayBuffer[Int]()
    val categories = new ArrayBuffer[ArrayBuffer[Int]]()

    train_lines.foreach{
      s =>
        if (!s.isEmpty()) {
          val line = s.split(" ")
          current_categories += line(1).toInt
        }
        else{
          categories.append(current_categories)
          current_categories = new ArrayBuffer[Int]()
        }
    }
    return categories
  }

  def maxTab(tab : Array[Double]) : Double = {
    var max = tab(0)
    for (i <- 0 until tab.length){
      if (tab(i) > max){
        max = tab(i)
      }
    }
    return max
  }

  def argmaxTab(tab : Array[Double]) : Int = {
		var argmax = 0
		var max = Double.NegativeInfinity
    for (i <- 0 until tab.length){
      if (tab(i) > max){
				max = tab(i)
				argmax = i
      }
    }
    return argmax
  }

  def tabFusion(tab : ArrayBuffer[ArrayBuffer[Int]]) : Array[Int] = {
    val result = new ArrayBuffer[Int]()
    for (i <- 0 until tab.length){
      for (j <- 0 until tab(i).length){
        result.append(tab(i)(j))
      }
    }
    val resultArray = new Array[Int](result.size)
    for (i <-0 until result.size){
      resultArray(i) = result(i)
    }
    return resultArray
  }
}
