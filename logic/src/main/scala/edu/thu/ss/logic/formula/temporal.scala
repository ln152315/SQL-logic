package edu.thu.ss.logic.formula

trait TemporalFormula {

}

/**
 * LTL temporal part
 */

case class AG(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "AG"

}

case class AF(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "AF"

}

case class AX(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "AX"

}

case class AU(left: Formula, right: Formula) extends BinaryFormula with TemporalFormula {
  val nodeName = "AU"

}

case class EG(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "EG"

}

case class EF(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "EF"

}

case class EX(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "EX"

}

case class EU(left: Formula, right: Formula) extends BinaryFormula with TemporalFormula {
  val nodeName = "EU"

}

/**
 * LTL temporal part
 */

case class pAG(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pAG"

}

case class pAF(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pAF"

}

case class pAX(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pAX"

}

case class pAS(left: Formula, right: Formula) extends BinaryFormula with TemporalFormula {
  val nodeName = "pAS"

}

case class pEG(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pEG"

}

case class pEF(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pEF"

}

case class pEX(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pEX"

}

case class pES(left: Formula, right: Formula) extends BinaryFormula with TemporalFormula {
  val nodeName = "pES"

}

/**
 * LTL temporal part
 */

case class G(interval: Seq[String], child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "G"
  override def toString = s"$nodeName[${interval.mkString(", ")}] $child"
  
}

case class F(interval: Seq[String], child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "F"
  override def toString = s"$nodeName[${interval.mkString(", ")}] $child"
  
}

case class X(interval: Seq[String], child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "X"
  var lower = interval.head.toInt
  var upper = interval.last.toInt
  override def toString = s"$nodeName[$lower, $upper] $child"
  
}

case class U(interval: Seq[String], left: Formula, right: Formula) extends BinaryFormula with TemporalFormula {
  val nodeName = "U"
  var newListSatisfied = 0.toLong
  var lower = interval.head.toLong
  var upper = interval.last.toLong
  override def toString = s"($left $nodeName[$lower, $upper] $right)"
  
}

/**
 * LTL temporal part
 */

case class pG(interval: Seq[String], child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pG"
  override def toString = s"$nodeName[${interval.mkString(", ")}] $child"
  
}

case class pF(interval: Seq[String], child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pF"
  override def toString = s"$nodeName[${interval.mkString(", ")}] $child"
  
}

case class pX(interval: Seq[String], child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pX"
  var lower = interval.head.toInt
  var upper = interval.last.toInt
  override def toString = s"$nodeName[$lower, $upper] $child"
  
}

case class pU(interval: Seq[String], left: Formula, right: Formula) extends BinaryFormula with TemporalFormula {
  val nodeName = "pU"
  var newListSatisfied = 0.toLong
  var lower = interval.head.toLong
  var upper = interval.last.toLong
  override def toString = s"($left $nodeName[$lower, $upper] $right)"
}

case class Count(interval: Seq[String], child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "Count"
  override def toString = s"$nodeName(${interval.mkString(", ")}) $child"
  
}

