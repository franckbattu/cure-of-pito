package models

import scala.collection.mutable.ArrayBuffer

case class Spell(var name: String, var level: scala.collection.mutable.Map[String, Int], var components: ArrayBuffer[String], var resistance: Boolean) {

  override def toString: String = s"Spell(name=${this.name}, level=${this.level}, components=${this.components}, resistance=${this.resistance})"
}
