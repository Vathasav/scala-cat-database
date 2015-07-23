package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current





// defining cat model
case class Cat(id: Option[Long] = None, name: String, color: String, race: String, gender: String, picture: Option[Array[Byte]])


//object for managing cat operations
object Cat {
  
 
  
 /**
  *Parser for transforming a ResultSet to Cat value 
  */
  
  val cat = {
  get[Option[Long]]("id") ~ 
  get[String]("name") ~
  get[String]("color") ~
  get[String]("race") ~
  get[String]("gender") ~
  get[Option[Array[Byte]]]("picture") map {
    case id~name~color~race~gender~picture => Cat(id, name, color, race, gender, picture)
  }
}

/**
  *Lists all cats in the database
  * Returns List[Cat]
  */

  def all(): List[Cat] = DB.withConnection { implicit connection =>
  SQL("select * from cat").as(cat *)
  }

  /**
   * Insert a new cat.
   *
   * @param cat The values of new cat.
   */
  def create(cat: Cat) = {
    DB.withConnection { implicit connection =>
      SQL(
        "insert into cat values ((select next value for cat_id_seq), {name}, {color}, {race}, {gender}, {picture})"
      ).on(
        'name -> cat.name,
        'color -> cat.color,
        'race -> cat.race,
        'gender -> cat.gender,
        'picture -> cat.picture
      ).executeUpdate()
    }
    
    println("success");
  }
  
  
    /**
   * Delete a cat.
   *
   * @param id Id of the cat to delete.
   */
  def delete(id: Long) = {
    DB.withConnection { implicit connection =>
      SQL("delete from cat where id = {id}").on('id -> id).executeUpdate()
    }
  }
  
  /**
   * Retrieve a cat from the id.
   */
  def findById(id: Long): Option[Cat] = {
    DB.withConnection { implicit connection =>
      SQL("select * from cat where id = {id}").on('id -> id).as(Cat.cat.singleOpt)
    }
  }
  
  /**
   * Update cat.
   *
   * @param id cat id
   * @param cat cat information.
   */
  def update(id: Long, cat: Cat) = {
    DB.withConnection { implicit connection =>
      SQL(
        " update cat set name = {name}, color = {color}, race = {race}, gender = {gender}, picture = {picture} where id = {id}").
        on(
        'id -> id,
        'name -> cat.name,
        'color -> cat.color,
        'race -> cat.race,
        'gender -> cat.gender,
        'picture -> cat.picture
      ).executeUpdate()
    }
  }
  
  
}

    /*
    *Popular list of 10 cat breeds obtained from Web
    */
    object CatBreeds{
        
       val list = List ("Abyssinian","American Shorthair","Persian","Maine Coon","Exotic","Siamese","Birman","Oriental Shorthair","Sphynx","Ragdoll")
        
    }
    
    /**
     * List of colors collected from web
     */ 
    object Colors{
        
        val list = List ("aliceblue", "antiquewhite", "aqua", "aquamarine", "azure", "beige", "bisque", "black", "blanchedalmond", "blue", "blueviolet", "brown", "burlywood", "cadetblue", "chartreuse", "chocolate", "coral", "cornflowerblue", "cornsilk", "crimson", "cyan", "darkblue", "darkcyan", "darkgoldenrod", "darkgray", "darkgreen", "darkkhaki", "darkmagenta", "darkolivegreen", "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategray", "darkturquoise", "darkviolet", "deeppink", "deepskyblue", "dimgray", "dodgerblue", "firebrick", "floralwhite", "forestgreen", "fuchsia", "gainsboro", "ghostwhite", "gold", "goldenrod", "gray", "green", "greenyellow", "honeydew", "hotpink", "indianred", "indigo", "ivory", "khaki", "lavender", "lavenderblush", "lawngreen", "lemonchiffon", "lightblue", "lightcoral", "lightcyan", "lightgoldenrodyellow", "lightgreen", "lightgrey", "lightpink", "lightsalmon", "lightseagreen", "lightskyblue", "lightslategray", "lightsteelblue", "lightyellow", "lime", "limegreen", "linen", "magenta", "maroon", "mediumaquamarine", "mediumblue", "mediumorchid", "mediumpurple", "mediumseagreen", "mediumslateblue", "mediumspringgreen", "mediumturquoise", "mediumvioletred", "midnightblue", "mintcream", "mistyrose", "moccasin", "navajowhite", "navy", "oldlace", "olive", "olivedrab", "orange", "orangered", "orchid", "palegoldenrod", "palegreen", "paleturquoise", "palevioletred", "papayawhip", "peachpuff", "peru", "pink", "plum", "powderblue", "purple", "red", "rosybrown", "royalblue", "saddlebrown", "salmon", "sandybrown", "seagreen", "seashell", "sienna", "silver", "skyblue", "slateblue", "slategray", "snow", "springgreen", "steelblue", "tan", "teal", "thistle", "tomato", "turquoise", "violet", "wheat", "white", "whitesmoke", "yellow", "yellowgreen")
    }
    
    // list for Gender
    object Gender {
    
        val list = List("Male","Female")
    }
