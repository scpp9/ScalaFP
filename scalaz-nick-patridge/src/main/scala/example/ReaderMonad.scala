package example

class ReaderMonad {

  case class User(id: Long, parentId: Long, name: String)
  trait UserRepo {
    def get(id: Long) : User
    def find(name: String): User
  }

  trait Users {
    def getUser(id: Long) : UserRepo => User = {
      case repo => repo.get(id)
    }
    def findUser(name: String): UserRepo => User = {
      case repo => repo.find(name)
    }
  }



}













