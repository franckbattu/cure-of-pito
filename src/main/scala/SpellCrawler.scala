import models.Spell
import org.jsoup.Jsoup
import org.jsoup.select.Elements

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer;

class SpellCrawler {

  val documentLink = "https://www.d20pfsrd.com/magic/all-spells/"
  val spellsQuery = "table a[href*=magic/all-spells]"
  val abstractQuery = "div.article-content p:not([class]):contains(School)"
  val componentsQuery = "div.article-content p:not([class]):contains(Components), div.article-content p:not([class]):contains(component)"
  val resistanceQuery = "div.article-content p:not([class]):contains(Resistance)"
  val descriptionQuery = "p:contains(DESCRIPTION) ~ *"
  val schoolsQuery = "p:contains(School) a[href*=magic#TOC]"

  /**
   * Crawl tous les spells
   * On récupère le nom, les levels, les composantes, la résistance, la description et les écoles d'un sort
   * @return un ArrayBuffer contenant tous les Spell
   */
  def crawl(): ArrayBuffer[Spell] = {
    println("Start crawling spells")

    val spells: ArrayBuffer[Spell] = new ArrayBuffer[Spell]()
    val document = Jsoup.connect(this.documentLink).ignoreHttpErrors(true).get()
    val links = document.select(this.spellsQuery)
    links.forEach(link => {
      val documentSpell = Jsoup.connect(link.attr("href")).ignoreHttpErrors(true).get()
      val name = documentSpell.select("h1").text().toLowerCase()
      val level = this.getLevel(documentSpell.select(this.abstractQuery).get(0).text())

      var components = new ArrayBuffer[String]()
      if (documentSpell.select(this.componentsQuery).size() > 0) {
        components = this.getComponents(documentSpell.select(this.componentsQuery).get(0).text())
      }

      var resistance = false
      if (documentSpell.select(this.resistanceQuery).size > 0) {
        resistance = this.getResistance(documentSpell.select(this.resistanceQuery).get(0).text())
      }

      var description = ""
      if (documentSpell.select(this.descriptionQuery).size > 0) {
        description = this.getDescription(documentSpell.select(this.descriptionQuery).get(0).text())
      }
      val schools = this.getSchools(documentSpell.select(this.schoolsQuery))

      val spell = Spell(name, schools, level, components, resistance, description)
      spells += spell
    })
    println("Stop crawling spells")
    spells
  }

  /**
   * Retourne le level d'un spell
   *
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
   * Retourne les écoles disponibles d'un sort
   *
   * @param elements la liste des éléments (type a[href]) à analyser
   * @return un ArrayBuffer de String (écoles) sans doublons
   */
  def getSchools(elements: Elements): ArrayBuffer[String] = {
    val result = new ArrayBuffer[String]()
    elements.forEach(element => result += element.text())
    result.distinct
  }

  /**
   * Retourne les composantes d'un Spell.
   *
   * @param sentence la phrase à analyser
   * @return un ArrayBuffer contenant toutes les composantes
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
      case None =>
    }
    result
  }

  /**
   * Retourne la résistance d'un spell
   *
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

  /**
   * Retourne la description d'un sort
   *
   * @param sentence la phrase à analyser
   * @return la description d'un sort
   */
  def getDescription(sentence: String): String = {
    sentence
  }
}
