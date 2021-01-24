package bank

import bank.time.Date

// creates a new bank with no account nor history

class Bank() {
    // returns a list of every bank account in the bank. 
    //The returned list is sorted in alphabetical order based on customer name. 
    import scala.collection.mutable.ArrayBuffer
    private var accounts = ArrayBuffer.empty[BankAccount]
    private var entries = ArrayBuffer.empty[HistoryEntry]

    def allAccounts(): Vector[BankAccount] = accounts.toVector

    // returns the account holding the provided account number.
    def findByNumber(accountNbr: Int): Option[BankAccount] = accounts.find(x => x.accountNumber == accountNbr)   

    // returns a list of every account belonging to the customer with the provided id
    def findAccountsForHolder(id: Long): Vector[BankAccount] = accounts.filter(x => x.holder.id == id).toVector

    // returns a list of all customers dwhose names match the provided name pattern
    def findByName(namePattern: String): Vector[Customer] = accounts.map(x => x.holder).filter(p => p.name.contains(namePattern)).toVector

    // executes anevent in the bank. Returns a string describing whether the event was succesful or failed
    def doEvent(event: BankEvent, shouldWrite: Boolean, date: Date = Date.now()): String = {
        println(s"Current event: $event")
        val newHistoryEntry = HistoryEntry(date, event)
        // vill inte alltid skriva in ett event
        
        try {
            event match {
                case Deposit(account, amount) => { // bästa sättet att hantera att man får fel inmatning utan att koden ser ut som skit?
                    println(s"DEPOSIT RUNNING")
                    println(s"accNum: $account")
                    var acc = findByNumber(account).get
                    acc.deposit(amount)
                    
                    x(newHistoryEntry, shouldWrite)
                    "Transaktionen lyckades"
                }
                case Withdraw(account, amount) => {
                    println(s"Withdraw RUNNING")
                    println(s"accNum: $account")
                    var acc = findByNumber(account).get
                    if (acc.withDraw(amount)) {
                        
                        x(newHistoryEntry, shouldWrite)
                        "Transaktionen lyckades"
                    }
                    else "Transaktionen misslyckades. Otillräckligt saldo."
                }
                case Transfer(accFrom, accTo, amount) => {
                    var destAcc = findByNumber(accTo).get
                    var fromAcc = findByNumber(accFrom).get

                    if (fromAcc.withDraw(amount)) {
                        println(s"Transfer RUNNING")
                        destAcc.deposit(amount)
                        x(newHistoryEntry, shouldWrite)
                        "Transaktionen lyckades"
                    }
                    else "Transaktionen misslyckades. Otillräckligt saldo."
                }
                case NewAccount(id, name) => {
                    val owner = Customer(name, id)
                    val newAcc = BankAccount(owner)
                    accounts += newAcc
                    println(s"NewAccount RUNNING")
                    x(newHistoryEntry, shouldWrite)
                    "Nytt konto skapat med kontonummer: "
                }
                case DeleteAccount(account) => {
                    var acc = findByNumber(account).get
                    accounts -= acc
                    BankAccount.addNum(acc.accountNumber)
                    println(s" Delete RUNNING")
                    x(newHistoryEntry, shouldWrite)
                    "Förgöring lyckades"
                }
            }
        }
        catch {
            case _: Throwable => {
                "Transaktion misslyckades. Hittade ej konto"
            }
        }
        
    }

    def x(newHistoryEntry: HistoryEntry, shouldWrite: Boolean): Unit = {
        if (shouldWrite) {
            Mechanic.writeFile(newHistoryEntry)
        }
        entries += newHistoryEntry
    }   

    // returns a log of all changes to the bank's state.
    def history(): Vector[HistoryEntry] = entries.toVector

    // resets the bank to the state it had at the provided date.
    // returns a string desccribing whether the event was succesful or failed 
    def returnToState(returnDate: Date): String = {
        val entriesTilNow = Mechanic.readFile.filter(x => x.date.compare(returnDate) <= 0)        
        println(s"Entries til now: $entriesTilNow")
        val preAccs = allAccounts().map(_.accountNumber)
        
        accounts = ArrayBuffer.empty[BankAccount] // reset everything
        entries = ArrayBuffer.empty[HistoryEntry]
        Mechanic.eraseFile
        BankAccount.stackGone
        //for (entry <- entriesTilNow) doEvent(entry.event, true, entry.date) 
        entriesTilNow.foreach(x => doEvent(x.event, true, x.date)) 

        val afterAccs = allAccounts().map(_.accountNumber)

        ((preAccs.diff(afterAccs)).sorted.reverse.foreach(x => BankAccount.addNum(x))) 
         
        "Bank has succesfully returned to state"
    } // innane eller lika med datumet

    
}
