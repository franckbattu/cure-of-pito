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

final case class Search(spellName: String, components: Array[String], classes: Array[String])
final case class ResponseSearch(name: String, creatures: List[String])
final case class ResponseSearchList(data: Array[ResponseSearch])

class Server(val spellsWithCreatures: RDD[(String, String)], val spells: RDD[Spell]) {

  def run(): Unit = {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher
    implicit val searchFormat = jsonFormat3(Search)
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

              correctSpells = this.checkName(correctSpells, search)

              val rdd1 = correctSpells.map(spell => (spell.name, spell.name))
              val rdd2: RDD[(String, Array[String])] = spellsWithCreatures.map { case (name, creatures) => (name, creatures.split(" / ")) }
              val rdd3 = rdd1.join(rdd2);

              val response: ResponseSearchList = ResponseSearchList(
                rdd3
                  .map { case (key, (name, creatures)) => println(name); ResponseSearch(name, creatures.toList) }
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

  def checkComponents(spells: RDD[Spell], search: Search): RDD[Spell] = {
    spells
      .filter(spell => spell.components.length == search.components.length)
      .filter(spell => search.components.map(component => spell.components.contains(component)).reduce((a, b) => a && b))
  }

  def checkClasses(spells: RDD[Spell], search: Search): RDD[Spell] = {
    spells
      .filter(spell => spell.level.size == search.classes.length)
      .filter(spell => spell.level.map { case (key, value) => search.classes.contains(key) }.reduce((a, b) => a && b))
  }

  def checkName(spells: RDD[Spell], search: Search): RDD[Spell] = {
    spells.filter(spell => spell.name.contains(search.spellName))
  }
}
