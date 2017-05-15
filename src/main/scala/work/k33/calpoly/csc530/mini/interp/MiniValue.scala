package work.k33.calpoly.csc530.mini.interp

import scala.collection.mutable

trait MiniValue

case class Num(num: Long) extends MiniValue {override def toString: String = num.toString}

case class Bool(bool: Boolean) extends MiniValue

case class Struct(fields: mutable.Map[String, MiniValue], var freed: Boolean) extends MiniValue

case class NullValue() extends MiniValue

case class VoidValue() extends MiniValue