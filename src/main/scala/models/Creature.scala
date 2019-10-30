package models

import scala.collection.mutable.ArrayBuffer

class Creature(val name : String) extends Serializable {
  var spells: ArrayBuffer[String] =  ArrayBuffer[String]()

  def addSpell(spell : String) : Unit = {
    spells += spell
  }

  override def toString: String = {
    s"Creature(name=${this.name}, spells=${this.spells})"
  }

}
