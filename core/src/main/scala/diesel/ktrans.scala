package diesel

import scala.annotation.compileTimeOnly

@compileTimeOnly("Enable macro paradise to expand macro annotations")
class ktrans extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val r = internal.KTransImpl.expand(defn)
//        println(r.syntax)
    r
  }

}
