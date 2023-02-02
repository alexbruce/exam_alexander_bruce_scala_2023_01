import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import scala.util.control.Breaks._

object AtmApp extends App {

    //case class User(val AccountOwnerID: String, val firstname: String, val surname: String, val mobile: String)
    enum Type:
        case Saving, Cheque
    val hello = 1

    class SavingAccount(val AccountOwnerID: String, val AccountNumber: String, val AccountType: String, var OpeningBalance: Double, val DatabaseIndex: Any = null)

    class ChequeAccount(val AccountOwnerID: String, val AccountNumber: String, val AccountType: String, var OpeningBalance: Double, val DatabaseIndex: Any = null)

    class User(val AccountOwnerID: String, val firstname: String, val surname: String, val mobile: String) {
      def readAccountOwner: String = AccountOwnerID
    }

    class ATM(var currentUser: User, var currentAccount: Any, var error: Boolean = false, var exitProgram: Boolean = false) {
      val accounts = importAccountDataTest().partitionMap({
          case cheque: ChequeAccount => Right(cheque)
          case saving: SavingAccount => Left(saving)
      })
      val savingAccounts = accounts._1
      val chequeAccounts = accounts._2
      println(savingAccounts)
      println(chequeAccounts)

      val users = importUserData()

      while !exitProgram do {
        breakable {
          val userID = readLine("Please enter your user ID: ")
          currentUser = login(userID, users)
          if currentUser == null then break
        }
      }

      def login(userID: String, userList: List[User]): User = {
        def tryLogin(): User = {
          try {
            userList.filter(_.AccountOwnerID == userID).head
          } catch {
            case e: Exception => null
          }
        }
        val loginAttempt = tryLogin()
        if loginAttempt == null then handleError("Mesesage")
        else println(s"Welcome, ${loginAttempt.firstname} ${loginAttempt.surname}.\nPlease select an option:\n\t1 for Deposit\n\t2 for Withdraw\n\t3 for Balance\n\tq to Quit")
        loginAttempt
      }

      def handleError(message: String): Unit = {
        println(s"Wrong Input\n\t$message")
      }

      def importAccountDataTest(): List[Any] = {
         Source.fromFile("data/OpeningAccountsData.txt").getLines().toList.drop(1).zipWithIndex.map({
              case (s"${number}|||${accNum}|||Saving|||${bal}",linenumber) => SavingAccount(number, accNum, "Saving", bal.toDouble, linenumber) ///s"Acc Num: $number \nAcc Numm: $accNum \nType: $accType\n Balance: $bal"
              case (s"${number}|||${accNum}|||Cheque|||${bal}",linenumber) => ChequeAccount(number, accNum, "Cheque", bal.toDouble, linenumber)
          })
      }

        def importUserData(): List[User] = {
          Source.fromFile("data/UserInfo.txt").getLines().toList.drop(1).map({
            case (s"${firstname},${surname},${mobile},${accountOwnerID}") => User(accountOwnerID, firstname, surname, mobile)
          })
        }

    }

    val atm = ATM(null, null)
}
//
