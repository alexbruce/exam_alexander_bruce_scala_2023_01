package au.com.binck.atmapp

import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.control.Breaks._
import java.io._

object AtmApp2 extends App {

  //case class User(val AccountOwnerID: String, val firstname: String, val surname: String, val mobile: String)
  enum Type:
    case Saving, Cheque

  class Account(val AccountOwnerID: String, val AccountNumber: String, val AccountType: String, var OpeningBalance: BigDecimal,  val DatabaseIndex: Int) {
    def changeBalance(accountBalance: BigDecimal, amount: BigDecimal): BigDecimal = {
      if ((amount <= 0 && accountBalance >= amount.abs) || (amount > 0)) then {
        accountBalance + amount
      } else {
        accountBalance
      }
    }
    def depositOrWithdraw(amount: BigDecimal): Unit = {
      OpeningBalance = changeBalance(OpeningBalance, amount)
      println(s"\nAccount Number: $AccountNumber ($AccountType):\n\tBalance: $$$OpeningBalance\n")
    }
  }

  class SavingAccount(AccountOwnerID: String, AccountNumber: String, AccountType: String, OpeningBalance: BigDecimal, DatabaseIndex: Int) extends Account(AccountOwnerID, AccountNumber, AccountType, OpeningBalance, DatabaseIndex)

  class ChequeAccount(AccountOwnerID: String, AccountNumber: String, AccountType: String, OpeningBalance: BigDecimal, DatabaseIndex: Int) extends Account(AccountOwnerID, AccountNumber, AccountType, OpeningBalance, DatabaseIndex)

  class User(val AccountOwnerID: String, val firstname: String, val surname: String, val mobile: String) {
    def readAccountOwner: String = AccountOwnerID
  }

  class ATM(var accounts: List[Account] = null, var users: List[User] = null, var currentUser: User = null, var currentAccount: Account = null, var error: Boolean = false, var exitProgram: Boolean = false) {
    accounts = importAccountData()
    println(accounts)
    println(accounts(0).AccountOwnerID)

    users = importUserData()

    def beginTransaction(): Unit = {
      while !exitProgram do {
        breakable {
          val userID = readLine("Please enter your user ID: ")
          currentUser = login(userID, users)
          if currentUser == null then break
          val matchedAccounts = findAccounts(currentUser, accounts)
          println(matchedAccounts)
          val transactionOption = selectTransaction(readLine(": "))
          if transactionOption == "q" then {
            quitSequence(accounts)
            break
          } else if transactionOption == null then {
            break
          }
          val accountOption = selectAccount(transactionOption, matchedAccounts)
          currentAccount = chooseAccount(accountOption, matchedAccounts)
          if currentAccount == null then break
          if transactionOption == "1" then {
            println("Please enter the amount to be deposited.")
            val validatedAmount = validateAmount(readLine(": $"))
            if validatedAmount == null then break
            currentAccount.depositOrWithdraw(validatedAmount)
            //println(accounts(currentAccount.DatabaseIndex).OpeningBalance)
            //accounts = accounts.updated(currentAccount.DatabaseIndex, currentAccount)
            //println(accounts(currentAccount.DatabaseIndex).OpeningBalance)
            //accounts(currentAccount.DatabaseIndex) = currentAccount
          } else if transactionOption == "2" then {
            println(s"Please enter the amount to be withdrawn. (Balance = ${currentAccount.OpeningBalance})")
            val validatedAmount = validateAmount("-" + readLine(": $"))
            if validatedAmount == null then break
            currentAccount.depositOrWithdraw(validatedAmount)
          } else if transactionOption == "3" then {
            println(s"Balance: $$${currentAccount.OpeningBalance}")
          }
        }
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
      if loginAttempt == null then handleError("Wrong user ID.")
      else println(s"Welcome, ${loginAttempt.firstname} ${loginAttempt.surname}.\nPlease select an option:\n\t1 for Deposit\n\t2 for Withdraw\n\t3 for Balance\n\tq to Quit")
      loginAttempt
    }

    def findAccounts(user: User, accounts: List[Account]): List[Account] = {
      try {
        accounts.filter(_.AccountOwnerID == user.AccountOwnerID)
      } catch {
        case e: Exception => {
          println(e)
          null
        }
      }
    }

    def selectTransaction(transactionOption: String): String = {
      if List("1", "2", "3", "q").contains(transactionOption) then {
        transactionOption
      } else {
        handleError("Please select an option from the list above")
        null
      }
    }

    def selectAccount(transactionOption: String, matchedAccounts: List[Account]): String = {
      if transactionOption == "2" then {
        matchedAccounts.zipWithIndex.foreach({
          case(a, i) => println((i + 1) + s" for ${a.AccountNumber} (${a.AccountType}). Balance: $$${a.OpeningBalance}")
        })
      } else {
        matchedAccounts.zipWithIndex.foreach({
          case (a, i) => println((i + 1) + s" for ${a.AccountNumber} (${a.AccountType})")
        })
      }
      readLine(": ")
    }

    def chooseAccount(accountOption: String, matchedAccounts: List[Account]): Account = {
      def tryAccount(): Account = {
        try {
          matchedAccounts(accountOption.toInt - 1)
        } catch {
          case e: Exception => null
        }
      }
      val accountAttempt = tryAccount()
      if accountAttempt == null then handleError("Mesesage")
      accountAttempt
    }

    def validateAmount(amount: String): BigDecimal = {
      def tryAmount(): BigDecimal = {
        try {
          BigDecimal(amount)
        } catch {
          case e: Exception => null
        }
      }
      val amountAttempt = tryAmount()
      if amountAttempt == null then handleError("Mesesage")
      amountAttempt
    }

    def quitSequence(accounts: List[Account]): Unit = {
      val accountsDataToWrite = "AccountOwnerID|||AccountNumber|||AccountType|||OpeningBalance\n" + createAccountWriteString(accounts, "|||")
      val pw = new PrintWriter(new File("data/OpeningAccountsData.txt" ))
      pw.write(accountsDataToWrite)
      println("AccountOwnerID\tAccountNumber\t\tAccountType\t\t\tOpeningBalance\n" + createAccountWriteString(accounts, "\t\t\t\t"))
      pw.close()
      exitProgram = true
    }

    def createAccountWriteString(accounts: List[Account], seperator: String): String = {
      def account2String(account: Account): String = {
        Array(account.AccountOwnerID, account.AccountNumber, account.AccountType, account.OpeningBalance).mkString(seperator)
      }

      accounts.map(account2String).mkString("\n")
    }

    def handleError(message: String): Unit = {
      println(s"Wrong Input\n\t$message")
    }

    def importAccountData(): List[Account] = {
      Source.fromFile("data/OpeningAccountsData.txt").getLines().toList.drop(1).zipWithIndex.map({
        case (s"${number}|||${accNum}|||Saving|||${bal}",linenumber) => SavingAccount(number, accNum, "Saving", BigDecimal(bal), linenumber) ///s"Acc Num: $number \nAcc Numm: $accNum \nType: $accType\n Balance: $bal"
        case (s"${number}|||${accNum}|||Cheque|||${bal}",linenumber) => ChequeAccount(number, accNum, "Cheque", BigDecimal(bal), linenumber)
      })
    }

    def importUserData(): List[User] = {
      Source.fromFile("data/UserInfo.txt").getLines().toList.drop(1).map({
        case (s"${firstname},${surname},${mobile},${accountOwnerID}") => User(accountOwnerID, firstname, surname, mobile)
      })
    }
  }
  @main def run(): Unit = {
    val atm = ATM()
    atm.beginTransaction()
  }
}