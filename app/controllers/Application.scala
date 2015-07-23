package controllers

import play.api._
import play.api.mvc._
import play.api.mvc.Result._
import play.api.data._
import play.api.data.Forms._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._


import models._
import models.Cat
import play.api.data.format.Formats._
import Gender._
import scala.io._
import java.io._

/**
 * Manage a database of cats
 */
class Application extends Controller {
    
   /**
   * Describe the cat form used in both add and edit screens.
   */
    val catForm: Form[Cat] = Form(
     mapping(
        "id" -> ignored(None:Option[Long]),
        "name" -> nonEmptyText(minLength = 2).verifying("Invalid pattern found", { txt =>
            val pattern = """[a-zA-Z]+"""
            txt.matches(pattern)}),
        "color" -> nonEmptyText,
        "race" -> nonEmptyText,
        "gender"->nonEmptyText,
        "picture"->ignored(Option.empty[Array[Byte]])
     )(Cat.apply)(Cat.unapply)
    )

   // -- Actions

  /**
   * Handle default path requests, redirect to cats list
   */  
  def index = Action {
 
      Redirect(routes.Application.cats)

  }
 
 /**
   * Display list of cats in the database
   */
  def cats = Action {
    Ok(views.html.index(Cat.all()))
  }

  /**
   * Display new cat form
   */
  def create = Action {
      Ok(views.html.addcat(catForm))
  }
  
  /**
   * Handle deletion of cat from database
   */
  def deleteCat(id: Long) = Action {
    Cat.delete(id)
    println(" Cat with "+id + "has been deleted")
    Redirect(routes.Application.cats)
  }
  
  /**
   * Display the editform of an existing cat
   */
  def editCat (id: Long) = Action {
      Cat.findById(id).map { cat =>
      Ok(views.html.editcat(cat, catForm.fill(cat)))
    }.getOrElse(NotFound)
      
  
  }
  
  /**
   * Handle edit form submission
   * 
   */
  def updateCat (id: Long) = Action (parse.multipartFormData) {
    
    // if picture is available use it for updating cat 
    // otherwise update cat with previous picture (if available)  
      implicit request =>
    request.body.file("imageFile").map { picture =>
  
  
   // load the image into binary array -- obtained from play documentation --
    val bis = new BufferedInputStream(new FileInputStream(picture.ref.file))
    val bArray = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
    
    // if form is bad return badrequest else process the data
    catForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.editcat(Cat.findById(id).get,formWithErrors)),

    cat => {
      
      // copy new cat from old cat but with new picture
      val newCat = cat.copy(picture = Some(bArray))
      
      //update cat
      Cat.update(id, newCat)
      
      println("update cat wit image"+cat.name)
      
      //redirect to front page
      Redirect(routes.Application.cats)
    }
    )
  }.getOrElse(
      
      // if no picture is found update cat with previous picture
      
     catForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.editcat(Cat.findById(id).get, formWithErrors)),

     cat => {
      
     // get cat from DB
     val catfromDB: Cat = Cat.findById(id).get
     
     //Add old cat's picture to new cat if aavailable and update in DB
     if(!catfromDB.picture.isEmpty){
         val newcat = cat.copy(picture=Cat.findById(id).get.picture)
         Cat.update(id, newcat)
     }
     else{
         Cat.update(id, cat)
     }
      
      
      println("added cat - without image"+cat.name)
      
      Redirect(routes.Application.cats)
    }
    )
  )

  }
  
  
  /**
   * return image for a cat
   */
  
  def getImage(id:Long) = Action{
  
  Cat.findById(id).map { cat =>
  
    val MimeType = "image/png"
        try {
               val imageData: Array[Byte] = cat.picture.get
                Ok(imageData).as(MimeType)
            }
        catch {
                case e: IllegalArgumentException =>
                BadRequest("Couldn’t generate image. Error: " + e.getMessage)
            }
      
    }.getOrElse(NotFound)
    
  }
    
    
  /**
   * Handle the new cat form submission
   * 
   * if picture is available use it
   * otherwise insert cat without picture
   */
    def newCat = Action(parse.multipartFormData) { implicit request =>
    
    //if picture is available add it
     request.body.file("imageFile").map { picture =>
    
    
   // load the image into binary array --obtained from play documentation--
    val bis = new BufferedInputStream(new FileInputStream(picture.ref.file))
    val bArray = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
    
    catForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.addcat(formWithErrors)),

    cat => {
       
      // update newcat from form data but with new picture
      val addCat = cat.copy(picture = Some(bArray))
       
      // insert cat into DB
      Cat.create(addCat) //, Some(bArray))
     
      println("added cat"+cat.name)
      Redirect(routes.Application.cats)
    }
    )
  }.getOrElse(
      
     // if no picture is found add cat to DB without image data
      
     catForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.addcat(formWithErrors)),

   cat => {
      
      Cat.create(cat)
      println("added cat"+cat.name)
      Redirect(routes.Application.cats)
    }
    )
    )
  }


}
