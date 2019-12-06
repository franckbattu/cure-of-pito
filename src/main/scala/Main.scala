import models.{Creature, Spell}
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ArrayBuffer

object Main extends App {

  val conf: SparkConf = new SparkConf()
    .setAppName("Guerison")
    .setMaster("local[*]")
    .set("spark.driver.host", "localhost");

  val sc: SparkContext = new SparkContext(conf)
  sc.setLogLevel("ERROR")

  val spellCrawler: SpellCrawler = new SpellCrawler()
  val spells: ArrayBuffer[Spell] = spellCrawler.crawl()
  val spellsRDD: RDD[Spell] = sc.makeRDD(spells)

  val creatureCrawler: CreatureCrawler = new CreatureCrawler()
  val creatures: ArrayBuffer[Creature] = creatureCrawler.crawl()
  val creaturesRDD: RDD[Creature] = sc.makeRDD(creatures)

  // Index inversé (spell -> creature)
  val spellsWithCreatures: RDD[(String, ArrayBuffer[String])] = creaturesRDD
    .map(creature => (creature.name, creature.spells))
    .reduceByKey((spells1, spells2) => spells1)
    .flatMap { case (name, spells) => spells.map(spell => (spell, ArrayBuffer(name))) }
    .reduceByKey((acc, name) => {
      if (!acc.contains(name(0))) {
        acc ++ name
      }
      else {
        acc
      }
    })

  // Sauvegarder dans un fichier texte
  // spellsWithCreatures.coalesce(1).saveAsTextFile("spellsWithCreatures")

  // Affichage de l'index inversé
  spellsWithCreatures
    .foreach(res => {
      println("Spell : " + res._1 + " | Creatures : " + res._2)
    })

  // Lancement du serveur
  val server = new Server(spellsWithCreatures, spellsRDD)
  server.run()

}
