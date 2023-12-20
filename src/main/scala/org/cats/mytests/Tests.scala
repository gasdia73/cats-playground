package org.cats.mytests

object Tests extends App {
  // Define a very simple JSON AST
  sealed trait Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String) extends Json
  final case class JsNumber(get: Double) extends Json
  final case object JsNull extends Json
  // The "serialize to JSON" behaviour is encoded in this trait

  trait JsonWriter[A] {
    def write(value: A): Json
  }
  final case class Person(name: String, email: String)

  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }

  import org.cats.mytests.InterfaceObjects.JsonWriterInstances._
  println(Json.toJson(Person("Dave", "dave@example.com")))

  //recursive implicits resolution
  implicit def optionWriter[A](
    implicit writer: JsonWriter[A]
  ): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      def write(option: Option[A]): Json =
        option match {
          case Some(aValue) => writer.write(aValue)
          case None         => JsNull
        }
    }
  println(Json.toJson(Option("A string")))

}
