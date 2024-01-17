import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import scala.util.matching.Regex
import java.security.Policy

object dicts {
  val es = Map(
    ("home" -> "case"),
    ("human" -> "hombre"),
    ("world" -> "muondo"),
    ("air" -> "air"),
    ("water" -> "agua")
  )
  val fr = Map(
    "home" -> "maison",
    "human" -> "homme",
    "world" -> "monde",
    "air" -> "air",
    "water" -> "eau"
  )
  val it = Map(
    "home" -> "case",
    "human" -> "Uomo",
    "world" -> "mondo",
    "air" -> "aria",
    "water" -> "aqua"
  )
  val pl = Map(
    "home" -> "dom",
    "human" -> "czlowiek",
    "world" -> "siat",
    "air" -> "powietrze",
    "water" -> "woda"
  )
}

object Formatters {

  def formatReplyTranslation(
      originalWord: String,
      languageName: String,
      res: String
  ) = {
    ("translation", originalWord, languageName, res)
  }
  def formatReplyOk(originalWord: String, languageName: String) = {
    ("yes", originalWord, languageName)
  }

  def formatReplyNotOk() = "no"

}

class ItalianWorker extends Actor {
  def languageName = "italian"
  def receive = {
    case ("do you recognize this word", s: String) => {
      // println(s"received question if I recognize word: $s")
      dicts.it.get(s) match {
        case Some(_res) =>
          sender ! Formatters.formatReplyOk(s, languageName)
        // case Some(res) => sender ! formatReplyOk(languageName, res)
        case None => sender ! Formatters.formatReplyNotOk
      }
    }
    case ("give me translation of this word", s: String) => {
      // println(s"received translation request for word: $s")

      val Some(word) = dicts.pl.get(s)
      sender ! Formatters.formatReplyTranslation(s, languageName, word)
    }
    case s => {
      println(s"received unknown message: $s")
    }
  }
}

class FrenchWorker extends Actor {
  def languageName = "french"
  def receive = {
    case ("do you recognize this word", s: String) => {
      // println(s"received question if I recognize word: $s")
      dicts.fr.get(s) match {
        case Some(_res) =>
          sender ! Formatters.formatReplyOk(s, languageName)
        // case Some(res) => sender ! formatReplyOk(languageName, res)
        case None => sender ! Formatters.formatReplyNotOk
      }
    }
    case ("give me translation of this word", s: String) => {
      // println(s"received translation request for word: $s")

      val Some(word) = dicts.pl.get(s)
      sender ! Formatters.formatReplyTranslation(s, languageName, word)
    }
    case s => {
      println(s"received unknown message: $s")
    }
  }
}

class PolishWorker extends Actor {
  def languageName = "polish"
  def receive = {
    case ("do you recognize this word", s: String) => {
      // println(s"received question if I recognize word: $s")
      dicts.pl.get(s) match {
        case Some(_res) =>
          sender ! Formatters.formatReplyOk(s, languageName)
        // case Some(res) => sender ! formatReplyOk(languageName, res)
        case None => sender ! Formatters.formatReplyNotOk
      }
    }
    case ("give me translation of this word", s: String) => {
      // println(s"received translation request for word: $s")

      val Some(word) = dicts.pl.get(s)
      sender ! Formatters.formatReplyTranslation(s, languageName, word)
    }
    case s => {
      println(s"received unknown message: $s")
    }
  }
}

class MainActor(workers: List[ActorRef]) extends Actor {
  def receive = {
    case ("do you recognize this word", word: String) => {
      // println(s"I have received $word");
      this.workers.foreach(worker =>
        worker ! ("do you recognize this word", word)
      )
    }
    case ("yes", originalWord, languageName) => {
      // println(s"found translation in language: $languageName")
      sender ! ("give me translation of this word", originalWord)
    }
    case "no" => {
      println("failed to find translation")
    }
    case ("translation", originalWord, languageName, translation) => {
      println(
        s"the translation of $originalWord in $languageName is: $translation"
      )
    }
    case s => {
      println(s"main actor received unrecognized message: $s")
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")

    val system = ActorSystem("Default")
    val polishWorker =
      system.actorOf(Props[PolishWorker], name = "polishWorker")

    val italianWorker =
      system.actorOf(Props[ItalianWorker], name = "italianWorker")
    val frenchWorker =
      system.actorOf(Props[FrenchWorker], name = "frenchWorker")
    val mainActor = system.actorOf(
      Props(new MainActor(List(polishWorker, italianWorker, frenchWorker))),
      name = "mainActor"
    )
    mainActor ! (("do you recognize this word", "home"))

    // val printer = system.actorOf(Props[PrinterActor], name = "X")

    // printer ! "starting"
    // printer ! "just a message"
  }
}

class PrinterActor extends Actor {
  def receive = {
    case "starting" => println("..")
    case s: String  => println("printing: " + s)
  }
}
