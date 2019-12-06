import models.Creature
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.collection.mutable.ArrayBuffer

class CreatureCrawler {

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
   * Crawl tous les bestiaires disponibles afin de fournir les créatures
   * On récupère le nom, et les spells de la créature
   *
   * @return un ArrayBuffer contenant toutes les créatures trouvées
   */
  def crawl(): ArrayBuffer[Creature] = {
    println("Start crawling creatures")

    val creatures: ArrayBuffer[Creature] = ArrayBuffer[Creature]();
    for (bestiary <- this.bestiaries) {
      println(bestiary)
      val document = Jsoup.connect(bestiary).get()
      val links: Elements = document.select("div.page-center a[href*=bestiary/monster-listings]")

      links.forEach(link => {
        try {
          val (name, section, creatureLink) = this.getDataCreature(link)
          val creature = new Creature(name)
          if (section != null) {
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

    println("Stop crawling creatures")
    creatures
  }

  /**
   * Donne toutes les informations d'une créature
   * La méthode est récursive au cas-où une créature ait changé d'adresse URL (dans ce cas, la méthode est appelée avec le nouveau lien)
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
}
