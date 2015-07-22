package controllers

import play.api._
import play.api.mvc._
import play.api.mvc.Result._
import play.api.data._
import play.api.data.Forms._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
//import play.api.mvc.MultiPartFormData.FilePart_

//import anorm._


import models._
import models.Cat
import play.api.data.format.Formats._
import Gender._
import scala.io._
import java.io._


class Application extends Controller {
    
   

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
 
  def index = Action {
 
      Redirect(routes.Application.cats)

  }
 
  def cats = Action {
    Ok(views.html.index(Cat.all()))
}

  def create = Action {
      Ok(views.html.addcat(catForm))
  }
  
  def deleteCat(id: Long) = Action {
    Cat.delete(id)
    println(" Cat with "+id + "has been deleted")
    Redirect(routes.Application.cats)
  }
  
  def editCat (id: Long) = Action {
      Cat.findById(id).map { cat =>
      Ok(views.html.editcat(cat, catForm.fill(cat)))
    }.getOrElse(NotFound)
      
  
  }
  
  def updateCat (id: Long) = Action (parse.multipartFormData) {
      
      implicit request =>
  request.body.file("imageFile").map { picture =>
    // retrieve the image and put it where you want...
    
    val filename = picture.filename
  
    val imageFile = new java.io.File(filename)
    picture.ref.moveTo(imageFile)
  
    val bis = new BufferedInputStream(new FileInputStream(imageFile))
    val bArray = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
    // handle the other form data
    catForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.editcat(Cat.findById(id).get,formWithErrors)),

   cat => {
      
      val newCat = cat.copy(picture = Some(bArray))
      
      Cat.update(id, newCat)
      println("update cat wit image"+cat.name)
      Redirect(routes.Application.cats)
    }
    )
  }.getOrElse(
      
     catForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.editcat(Cat.findById(id).get, formWithErrors)),

   cat => {
       
     val catfromDB: Cat = Cat.findById(id).get
     
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
    
    
  
def newCat = Action(parse.multipartFormData) { implicit request =>
  request.body.file("imageFile").map { picture =>
    // retrieve the image and put it where you want...
    
    val imageFile = new java.io.File("myFileName")
    picture.ref.moveTo(imageFile)
    
    
    val bis = new BufferedInputStream(new FileInputStream(imageFile))
    val bArray = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
    // handle the other form data
    catForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.addcat(formWithErrors)),

   cat => {
      
      Cat.create(cat, Some(bArray))
      println("added cat"+cat.name)
      Redirect(routes.Application.cats)
    }
    )
  }.getOrElse(
      
     catForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.addcat(formWithErrors)),

   cat => {
      
      Cat.create(cat, None)
      println("added cat"+cat.name)
      Redirect(routes.Application.cats)
    }
    )
      
      
      )
}



  
  
 
  
/*  def deleteCat(id: Long) = Action {
    //Cat.delete(id)
    
    val bis = new BufferedInputStream(new FileInputStream(fileName))
val bArray = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
  }
  */

}
