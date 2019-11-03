import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import models.{Creature, Spell}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer;

class Crawler {

    val bestiaries = Array(
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-a-b/",
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-c-d/",
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-e-f/",
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-g-h/",
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-i-j/",
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-k-l/",
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-m-n/",
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-o-p/",
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-q-r/",
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-s-t/",
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-u-v/",
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-w-x/",
      "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-y-z/"
    )

  /**
   * Crawl all the bestiaries
   * @return an ArrayBuffer of all creatures with their spells
   */
  def crawlBestiaries(): ArrayBuffer[Creature] = {

    val creatures: ArrayBuffer[Creature] = ArrayBuffer[Creature]();

    for (bestiary <- this.bestiaries) {
      println(bestiary)
      val document = Jsoup.connect(bestiary).get()
      val links: Elements = document.select("div.page-center a[href*=bestiary/monster-listings]")

      links.forEach(link => {
        try {
          val document: Document = Jsoup.connect(link.attr("href")).ignoreHttpErrors(true).get()
          val (name, section) = this.getDataBestiary(document)

          val creature = new Creature(name)

          section.forEach(spellLink => {
            creature.addSpell(spellLink.text().toLowerCase)
          })
          creatures += creature
        }
        catch {
          case e: Throwable => e.printStackTrace()
        }
      })
    }

    creatures
  }

  def getDataBestiary(document: Document): (String, Elements) = {
    val idArticle = document.select("article").attr("id")
    if (!idArticle.equals("post-404")) {
      val name = document.select("h1").text()
      val section = document.select("article div.statblock a[href*=all-spells]")
      (name, section)
    }
    else {
      // Problème avec https://www.d20pfsrd.com/bestiary/monster-listings/animals/cat-great/margay-tohc
      val link = document.select("article a").first()
      val newDoc = Jsoup.connect(link.attr("href")).ignoreHttpErrors(true).get()
      this.getDataBestiary(newDoc)
    }
  }

  def crawlSpells(): ArrayBuffer[Spell] = {
    val spells: ArrayBuffer[Spell] = new ArrayBuffer[Spell]()
    val document = Jsoup.connect("https://www.d20pfsrd.com/magic/all-spells/").ignoreHttpErrors(true).get()
    val links = document.select("table a[href*=magic/all-spells]")
    links.forEach(link => {
      val documentSpell = Jsoup.connect(link.attr("href")).ignoreHttpErrors(true).get()
      val name = documentSpell.select("h1").text().toLowerCase()
      val data = documentSpell.select("div.article-content p:not([class])")
      val level = this.getLevel(data.first().text())
      val components = this.getComponents(data.get(1).text())
      val resistance = if (data.size() >= 3 ) this.getResistance(data.get(2).text()) else false
      spells += new Spell(name, level, components, resistance)
    })
    spells;
  }

  def getLevel(sentence: String): Map[String, Int] = {
    val properties = sentence.split("; ")
    val levels = properties(properties.length-1).replace("Level ", "")
    val result = new mutable.HashMap[String, Int]()

    for(entry <- levels.split(", ")) {
      val tuple = entry.split(" ")
      if (tuple.length == 2) {
        result += ((tuple(0), tuple(1).toInt))
      }
    }
    result.toMap
  }

  def getComponents(sentence: String): ArrayBuffer[String] = {
    var result = new ArrayBuffer[String]()
    val matcher = "[A-Z]+".r
    for (component <- matcher.findAllIn(sentence)) {
      if (!(component.equals("C") || component.equals("T"))) {
        result += component
      }
    }
    result
  }

  def getResistance(sentence: String): Boolean = {
    val matcher = "(?<=Spell Resistance ).*".r
    matcher.findFirstIn(sentence) match {
      case Some(value: String) => if (value.length >= 3) { value.substring(0, 3).equals("yes") } else { false }
      case None => false
    }
  }
}
