package diesel

import scala.meta._

class diesel extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val r = diesel.internal.MacroImpl.expand(defn)
//    println(r.syntax)
    r
  }

}
