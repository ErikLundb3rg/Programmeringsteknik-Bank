package bank


// creates a bank account for the cusotmer provided.
// the account is given a unique acccount number and initially 
// has a balance of 0 kr


class BankAccount(val holder: Customer) {
    val accountNumber: Int = BankAccount.getNewNum()
    private var balance: BigInt = 0

    // deposits the provided amount in this account

    def deposit(amount: BigInt): Unit = {
        balance += amount
    }
    // returns the balance of this account
    def getBalance: Int = balance.toInt

    def withDraw(amount: BigInt): Boolean = {
        var isValid = false
        if (balance - amount >= 0) {
            isValid = true
            balance -= amount
        }
        isValid
    }

    // Withdraws the providded amount from this account 
    //if there is enough money in the account. Returns true if the
    // transaction was successful otherwise false

    override def toString(): String = s"Bankaccount accountnumber: $accountNumber, balance: $balance"
}

object BankAccount {
    import scala.collection.mutable.Stack 
    var freeNums = Stack[Int]() 

    for (i <- (1000 to 1500).reverse) {
        freeNums.push(i)
    }

    def apply(holder: Customer): BankAccount = new BankAccount(holder)

    def getNewNum(): Int = {
        val existingNums = BankApplication.bank.allAccounts().map(_.accountNumber) // Get all the existing numbers 
        println(s"Existing nums: $existingNums")
        if (!existingNums.contains(freeNums.head)) freeNums.pop()
        else getNewNum()
    } // if number   trying to get already exists 
    // then we get the next one instead
    def addNum(num: Int): Unit = freeNums.push(num) 
    // in case we delete an account we just add it back to the freeNums

    def getHead: Int = freeNums.head

    def stackGone: Unit = {
        freeNums = Stack[Int]()

        for (i <- (1000 to 1500).reverse) {
            freeNums.push(i)
        }
    }

 
    // check all current bankaccounts 
    // get all their bankNumbers
    // define localList 
    // current banknumbers should not be in localList
}