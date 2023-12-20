package org.cats.mytests

import org.cats.mytests.InterfaceObjects.JsonWriterInstances.personWriter
import org.cats.mytests.InterfaceSyntax.JsonSyntax.JsonWriterOps
import org.cats.mytests.Tests._

object InterfaceSyntax extends App {

  object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit w: JsonWriter[A]): Json =
        w.write(value)
    }
  }

  println(Person("Dave", "dave@example.com").toJson)

}
