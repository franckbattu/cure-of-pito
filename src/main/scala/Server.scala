import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import models.Spell
import org.apache.spark.rdd.RDD
import spray.json.DefaultJsonProtocol._

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

final case class Search(spellName: String, schools: Array[String], components: Array[String], classes: Array[String], description: String)

final case class ResponseSearch(name: String, creatures: List[String])

final case class ResponseSearchList(data: Array[ResponseSearch])

class Server(val spellsWithCreatures: RDD[(String, ArrayBuffer[String])], val spells: RDD[Spell]) {

  def run(): Unit = {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher
    implicit val searchFormat = jsonFormat5(Search)
    implicit val responseSearchFormat = jsonFormat2(ResponseSearch)
    implicit val responseSearchListFormat = jsonFormat1(ResponseSearchList)

    val route = {
      path("spells") {
        cors() {
          post {
            entity(as[Search]) { search => {
              var correctSpells: RDD[Spell] = spells;

              if (search.components.nonEmpty) {
                correctSpells = this.checkComponents(correctSpells, search)
              }

              if (search.classes.nonEmpty) {
                correctSpells = this.checkClasses(correctSpells, search)
              }

              if (search.description.length > 0) {
                correctSpells = this.checkDescription(correctSpells, search)
              }

              if (search.schools.length > 0) {
                correctSpells = this.checkSchools(correctSpells, search);
              }

              correctSpells = this.checkName(correctSpells, search)

              val rdd1 = correctSpells.map(spell => (spell.name, spell.name))
              val rdd2: RDD[(String, Array[String])] = spellsWithCreatures.map { case (name, creatures) => (name, creatures.toArray) }
              val rdd3 = rdd1.join(rdd2)

              val response: ResponseSearchList = ResponseSearchList(
                rdd3
                  .map { case (key, (name, creatures)) => ResponseSearch(name, creatures.toList) }
                  .collect()
              )
              complete(response)
            }
            }
          }
        }
      }
    }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8000)

    println(s"Server online at http://localhost:8000/\nPress RETURN to stop...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }

  /**
   * Retourne tous les spells contenant au moins toutes les composantes recherchées
   * @param spells RDD contenant les spells à analyser
   * @param search la liste des paramètres de recherche
   *
   * @return un RDD de Spell contenant au moins toutes les composantes recherchées
   */
  def checkComponents(spells: RDD[Spell], search: Search): RDD[Spell] = {
    spells
      .filter(spell => spell.components.length >= search.components.length)
      .filter(spell => {
        var counter = 0
        spell.components.foreach(component => if (search.components.contains(component)) counter += 1)
        counter == search.components.length
      })
  }

  /**
   * Retourne tous les spells contenant au moins toutes les classes recherchées
   *
   * @param spells RDD contenant les spells à analyser
   * @param search la liste des paramètres de recherche
   *
   * @return un RDD de Spell contenant au moins toutes les classes recherchées
   */
  def checkClasses(spells: RDD[Spell], search: Search): RDD[Spell] = {
    spells
      .filter(spell => spell.level.size >= search.classes.length)
      .filter(spell => {
        var counter = 0
        spell.level.keys.foreach(key => if (search.classes.contains(key)) counter += 1)
        counter == search.classes.length
      })
  }

  /**
   * Retourne tous les spells contenant au moins le nom recherché dans son nom
   *
   * @param spells RDD contenant les spells à analyser
   * @param search la liste des paramètres de recherche
   *
   * @return un RDD de Spell contenant au moins le nom recherché dans son nom
   */
  def checkName(spells: RDD[Spell], search: Search): RDD[Spell] = {
    spells.filter(spell => spell.name.contains(search.spellName))
  }

  /**
   * Retourne tous les spells contenant au moins les termes recherchés dans sa description
   *
   * @param spells RDD contenant les spells à analyser
   * @param search la liste des paramètres de recherche
   *
   * @return un RDD de Spell contenant au moins les termes recherchés dans sa description
   */
  def checkDescription(spells: RDD[Spell], search: Search): RDD[Spell] = {
    val words = search.description.split(" ")
    spells.filter(spell => words.map(word => spell.description.contains(word)).reduce((a, b) => a && b))
  }

  /**
   * Retourne tous les spells contenant au moins toutes les écoles recherchées
   *
   * @param spells RDD contenant les spells à analyser
   * @param search la liste des paramètres de recherche
   *
   * @return un RDD de Spell contenant au moins toutes les écoles recherchées
   */
  def checkSchools(spells: RDD[Spell], search: Search): RDD[Spell] = {
    spells
      .filter(spell => spell.schools.length >= search.schools.length)
      .filter(spell => {
        var counter = 0
        spell.schools.foreach(school => if (search.schools.contains(school)) counter += 1)
        counter == search.schools.length
      })
  }
}
