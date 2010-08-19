package pci

object Algorithms {
  import scala.math._
  import scala.collection.Map
  import scala.collection.mutable.HashMap

  def simDistance(prefs:Map[String, Map[String,Double]], person1:String, person2:String) = {
    val shareItems = HashMap[String,Int]()
    for {item <- prefs(person1)
         if prefs(person2).contains(item._1)} shareItems(item._1) = 1
    if (shareItems.size == 0) 0
    else {
      val sumOfSequares:Double = (for {
        item <- prefs(person1)
        if prefs(person2).contains(item._1)
      } yield pow(prefs(person1)(item._1) - prefs(person2)(item._1), 2) ).sum
      1 / (1 + sqrt(sumOfSequares))
    }
  }

  def simPearson(prefs:Map[String, Map[String, Double]], person1:String, person2:String): Double = {
    val shareItems = HashMap[String,Int]()
    for {item <- prefs(person1)
         if prefs(person2).contains(item._1)} shareItems(item._1) = 1

    val len = shareItems.size
    if (len == 0) return 0

    val sum1 = (for {item <- shareItems} yield prefs(person1)(item._1)).sum
    val sum2 = (for {item <- shareItems} yield prefs(person2)(item._1)).sum

    val sum1Sq = (for {item <- shareItems} yield pow(prefs(person1)(item._1), 2.0)).sum
    val sum2Sq = (for {item <- shareItems} yield pow(prefs(person2)(item._1), 2.0)).sum

    val pSum = (for {item <- shareItems} yield prefs(person1)(item._1) * prefs(person2)(item._1)).sum

    val num = pSum - (sum1 * sum2 / len)
    val den = sqrt((sum1Sq-pow(sum1,2.0)/len) * (sum2Sq-pow(sum2,2.0)/len))
    if (den == 0) 0 else num/den
  }

  def topMatcher(prefs:Map[String, Map[String, Double]], person:String, n:Int=5,
                 similarity:(Map[String, Map[String,Double]],String, String)=>Double = simPearson) = {
    val scores:Iterable[(Double,String)] = for {other <- prefs.keySet
                      if other != person} yield (similarity(prefs, person, other), other)
    scores.toList.sortWith(_._1 > _._1).take(n)
  }

  def getRecommendations(prefs:Map[String, Map[String, Double]], person:String,
                         similarity:(Map[String, Map[String,Double]],String, String)=>Double = simPearson) = {
    val totals = HashMap[String, Double]()
    val simSums = HashMap[String, Double]()

    for {other <- prefs.keySet
         if other != person
         val sim = similarity(prefs, person, other)
         if sim > 0
         item <- prefs(other)
         if !prefs(person).contains(item._1) || prefs(person)(item._1) == 0
    } {
      if (!totals.contains(item._1)) totals(item._1) = 0
      totals(item._1) += prefs(other)(item._1) * sim

      if (!simSums.contains(item._1)) simSums(item._1) = 0
      simSums(item._1) += sim
    }
    
    val ranking:Iterable[(Double,String)] = (for {(item,total) <- totals} yield (total/simSums(item), item))
    ranking.toList.sortWith(_._1 > _._1)
  }
}

object Recommendations {

  val critics = Map(
    "Jack Matthews" -> Map("Lady in the Water" -> 3.0, "Snakes on a Plane" -> 4.0, "You, Me and Dupree" -> 3.5,
                           "Superman Returns" -> 5.0, "The Night Listener" -> 3.0),
    "Mick LaSalle" -> Map("Lady in the Water" -> 3.0, "Snakes on a Plane" -> 4.0, "Just My Luck" -> 2.0,
                          "Superman Returns" -> 3.0, "You, Me and Dupree" -> 2.0, "The Night Listener" -> 3.0),
    "Claudia Puig" -> Map("Snakes on a Plane" -> 3.5, "Just My Luck" -> 3.0, "You, Me and Dupree" -> 2.5,
                          "Superman Returns" -> 4.0, "The Night Listener" -> 4.5),
    "Lisa Rose" -> Map("Lady in the Water" -> 2.5, "Snakes on a Plane" -> 3.5, "Just My Luck" -> 3.0,
                       "Superman Returns" -> 3.5, "The Night Listener" -> 3.0, "You, Me and Dupree" -> 2.5),
    "Toby" -> Map("Snakes on a Plane" -> 4.5, "Superman Returns" -> 4.0, "You, Me and Dupree" -> 1.0),
    "Gene Seymour" -> Map("Lady in the Water" -> 3.0, "Snakes on a Plane" -> 3.5, "Just My Luck" -> 1.5,
                          "Superman Returns" -> 5.0, "You, Me and Dupree" -> 3.5, "The Night Listener" -> 3.0),
    "Michael Phillips" -> Map("Lady in the Water" -> 2.5, "Snakes on a Plane" -> 3.0, "Superman Returns" -> 3.5,
                              "The Night Listener" -> 4.0))

  def transformPrefs(prefs:Map[String, Map[String, Double]]) = {
    import scala.collection.mutable.HashMap
    val result = HashMap[String, HashMap[String,Double]]()
    for {person <- prefs.keySet; item <- prefs(person)} {
      if (!result.contains(item._1)) result(item._1) = HashMap[String,Double]()
      result(item._1)(person) = prefs(person)(item._1)
    }
    result
  }

  def main(args: Array[String]): Unit = {
    import Algorithms._
    println(simDistance(critics, "Lisa Rose", "Gene Seymour"))
    println(simPearson(critics, "Lisa Rose", "Gene Seymour"))
    println(topMatcher(critics, "Toby", n=3))
    println("------------")
    println(getRecommendations(critics, "Toby"))
    println(getRecommendations(critics, "Toby", simDistance))
    println("------------")
    println(topMatcher(transformPrefs(critics), "Superman Returns"))
    println(getRecommendations(transformPrefs(critics), "Just My Luck"))
  }

}
