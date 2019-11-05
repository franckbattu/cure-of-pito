import models.Spell
import org.apache.spark.rdd.RDD
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import scala.io.StdIn
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._

class Server(val spellsWithCreatures: RDD[(String, String)], val spells: RDD[Spell]) {

  final case class SpellName(name: String)
  final case class SpellNames(spellNames: Array[SpellName])
  implicit val spellNameFormat = jsonFormat1(SpellName)
  implicit val spellNamesFormat = jsonFormat1(SpellNames)

  def run(): Unit = {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val route =
      path("hello") {
        get {
          val spellsNameList: Array[String] = spells.map(spell => spell.name).collect()
          complete(spellsNameList)
        }
      }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
