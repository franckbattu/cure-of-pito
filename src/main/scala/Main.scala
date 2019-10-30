import models.Creature
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ArrayBuffer

object Main extends App {

  val conf: SparkConf = new SparkConf()
    .setAppName("Guerison")
    .setMaster("local[*]")

  val sc: SparkContext = new SparkContext(conf)
  sc.setLogLevel("ERROR")

  val crawler: Crawler = new Crawler();

  // Crawling
  println("Start crawling creatures")
  val creatures: ArrayBuffer[Creature] = crawler.crawl()
  println("Stop crawling creatures")

  // (creature, spells)
  val rdd: RDD[Creature] = sc.makeRDD(creatures)

  // inverted indexes (spell, creatures)
  val spellsWithCreatures: RDD[(String, String)] = rdd
    .map(creature => (creature.name, creature.spells))
    .reduceByKey((spells1, spells2) => spells1)
    .flatMap{case (name, spells) => spells.map(spell => (spell, name))}
    .reduceByKey((name1, name2) => name1 + " / " + name2)

  // save as file
  // spellsWithCreatures.coalesce(1).saveAsTextFile("spellsWithCreatures")

  // display
  spellsWithCreatures
    .foreach(res => {
    println("Spell : " + res._1 + " | Creatures : " + res._2)
  })

}