import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
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

//  val bestiaries = Array(
//    "https://www.d20pfsrd.com/bestiary/bestiary-alphabetical/bestiary-a-b/",
//  )

  /**
   * Crawl toutes les bestiaries
   *
   * @return un ArrayBuffer de toutes les créatures avec leurs sorts
   */
  def crawlBestiaries(): ArrayBuffer[Creature] = {
    val creatures: ArrayBuffer[Creature] = ArrayBuffer[Creature]();
    for (bestiary <- this.bestiaries) {
      println(bestiary)
      val document = Jsoup.connect(bestiary).get()
      val links: Elements = document.select("div.page-center a[href*=bestiary/monster-listings]")

      links.forEach(link => {
        try {
          val (name, section, creatureLink) = this.getDataCreature(link)

          if (section != null) {
            val creature = new Creature(name, creatureLink)
            section.forEach(spellLink => {
              creature.addSpell(spellLink.text().toLowerCase)
            })
            creatures += creature
          }

        }
        catch {
          case e: Throwable => e.printStackTrace()
        }
      })
    }

    creatures
  }

  /**
   * Donne toutes les informations d'une créature
   * La méthode est récursive au cas-où une créature a changé d'adresse URL (dans ce cas, la méthode est appelée avec le nouveau lien)
   *
   * @param link
   * @return un Tuple contenant le nom de la créature, et un document contenant les informations
   */
  def getDataCreature(link: Element): (String, Elements, String) = {
    var creatureLink = link
    val document: Document = Jsoup.connect(creatureLink.attr("href")).ignoreHttpErrors(true).get()
    val idArticle = document.select("article").attr("id")
    if (!idArticle.equals("post-404")) {
      val name: String = document.select("h1").text()
      val section = document.select("article div.statblock a[href*=all-spells]")
      (name, section, creatureLink.text())
    }
    else {
      creatureLink = document.select("article a").first()
      if (creatureLink != null) {
        this.getDataCreature(creatureLink)
      }
      else {
        (null, null, null)
      }
    }
  }

  /**
   * Crawl tous les spells
   * @return un ArrayBuffer contenant tous les Spell
   */
  def crawlSpells(): ArrayBuffer[Spell] = {
    val spells: ArrayBuffer[Spell] = new ArrayBuffer[Spell]()
    val document = Jsoup.connect("https://www.d20pfsrd.com/magic/all-spells/").ignoreHttpErrors(true).get()
    val links = document.select("table a[href*=magic/all-spells]")
    links.forEach(link => {
      val documentSpell = Jsoup.connect(link.attr("href")).ignoreHttpErrors(true).get()
      val name = documentSpell.select("h1").text().toLowerCase()

      val level: mutable.Map[String, Int] = this.getLevel(documentSpell.select("div.article-content p:not([class]):contains(School)").get(0).text())

      var components = new ArrayBuffer[String]()
      if (documentSpell.select("div.article-content p:not([class]):contains(Components), div.article-content p:not([class]):contains(component)").size() > 0) {
        components = this.getComponents(documentSpell.select("div.article-content p:not([class]):contains(Components), div.article-content p:not([class]):contains(component)").get(0).text())
      }

      var resistance = false
      if (documentSpell.select("div.article-content p:not([class]):contains(Resistance)").size > 0) {
        resistance = this.getResistance(documentSpell.select("div.article-content p:not([class]):contains(Resistance)").get(0).text())
      }

      val spell = Spell(name, level, components, resistance)
      spells += spell
    })
    spells
  }

  /**
   * Retourne le level d'un spell
   * @param sentence la phrase à analyser
   * @return une Map[String, Int] associant le nom de la classe avec le niveau
   */
  def getLevel(sentence: String): mutable.HashMap[String, Int] = {
    val result = new mutable.HashMap[String, Int]()
    val matcher = "(?<=Level ).*".r
    matcher.findFirstIn(sentence) match {
      case Some(value: String) => {
        val properties = value.split("; ")
        for (entry <- properties(0).split(", ")) {
          val tuple = entry.split(" ")
          if (tuple.length == 2) {
            result += ((tuple(0), tuple(1).toInt))
          }
        }
        result
      }
      case None => result
    }
  }

  /**
   * Retourne les components d'un spell.
   * @param sentence la phrase à analyser
   * @return un ArrayBuffer contenant toutes les componenents
   */
  def getComponents(sentence: String): ArrayBuffer[String] = {
    var result = new ArrayBuffer[String]()
    val matcher1 = "(?<=Components|component ).*".r
    matcher1.findFirstIn(sentence) match {
      case Some(value: String) => {
        val matcher2 = "[A-Z]+".r
        for (component <- matcher2.findAllIn(value)) {
          if (!(component.equals("C") || component.equals("T"))) {
            result += component
          }
        }
      }
      case None => null
    }
    result
  }

  /**
   * Retourne la résistance d'un spell
   * @param sentence la phrase à analyser
   * @return un Boolean permettant de savoir si le spell a une résistance ou non
   */
  def getResistance(sentence: String): Boolean = {
    val matcher = "(?<=Spell Resistance ).*".r
    matcher.findFirstIn(sentence) match {
      case Some(value: String) => if (value.length >= 3) {
        value.substring(0, 3).equals("yes")
      } else {
        false
      }
      case None => false
    }
  }
}
