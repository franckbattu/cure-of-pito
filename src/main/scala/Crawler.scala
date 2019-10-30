import java.util.Objects

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import models.Creature

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

  def crawl(): ArrayBuffer[Creature] = {

    val creatures: ArrayBuffer[Creature] = ArrayBuffer[Creature]();

    for (bestiary <- this.bestiaries) {
      println(bestiary)
      val document = Jsoup.connect(bestiary).get()
      val links: Elements = document.select("div.page-center a[href*=bestiary/monster-listings]")

      links.forEach(link => {
        try {
          val document: Document = Jsoup.connect(link.attr("href")).ignoreHttpErrors(true).get()
          val (name, section) = this.getData(document)

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

  def getData(document: Document): (String, Elements) = {
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
      this.getData(newDoc)
    }
  }
}
