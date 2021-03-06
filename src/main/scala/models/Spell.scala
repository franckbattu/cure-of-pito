package models

import scala.collection.mutable.ArrayBuffer

case class Spell(var name: String,
                 val schools: ArrayBuffer[String],
                 var level: scala.collection.mutable.Map[String, Int],
                 var components: ArrayBuffer[String],
                 var resistance: Boolean,
                 var description: String) {

  override def toString: String = s"Spell(name=${this.name}, schools: ${this.schools}, level=${this.level}, components=${this.components}, resistance=${this.resistance}, description=${description})"
}
