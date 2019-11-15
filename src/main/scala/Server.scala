import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import models.Spell
import org.apache.spark.rdd.RDD
import spray.json.DefaultJsonProtocol._

import scala.io.StdIn

import scala.runtime.ScalaRunTime._

final case class Search(spellName: String, components: Array[String], classes: Array[String]) {
  override def toString: String = s"Search(spellName=${spellName}, components=${stringOf(components)}, classes=${stringOf(classes)})"
}

class Server(val spellsWithCreatures: RDD[(String, String)], val spells: RDD[Spell]) {

  def run(): Unit = {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher
    implicit val searchFormat = jsonFormat3(Search)

    val route = {
      path("spells") {
        cors() {
          post {
            entity(as[Search]) { search => {
              var correctSpells = spells;

              if (search.components.length > 0) {
                correctSpells = this.checkComponents(correctSpells, search)
              }

              if (search.classes.length > 0) {
                correctSpells = this.checkClasses(correctSpells, search)
              }

              correctSpells = this.checkName(correctSpells, search)

              complete(correctSpells
                .map(spell => spell.name)
                .collect())
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

  def checkComponents(spells: RDD[Spell], search: Search): RDD[Spell] = {
    spells
      .filter(spell => spell.components.length == search.components.length)
      .filter(spell => search.components.map(component => spell.components.contains(component)).reduce((a, b) => a && b))
  }

  def checkClasses(spells: RDD[Spell], search: Search): RDD[Spell] = {
    spells
      .filter(spell => spell.level.size == search.classes.length)
      .filter(spell => search.classes.map(classe => spell.level.contains(classe)).reduce((a, b) => a && b))
  }

  /**
   * Vérifie que le nom de la recherche soit contenu dans le nom du spell
   *
   * @param spells
   * @param search
   * @return
   */
  def checkName(spells: RDD[Spell], search: Search): RDD[Spell] = {
    spells.filter(spell => spell.name.contains(search.spellName))
  }
}
