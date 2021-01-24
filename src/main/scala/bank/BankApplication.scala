
package bank

import scala.io.StdIn.readLine
import bank.time.Date
import scala.annotation.tailrec

object BankApplication {

    val bank = new Bank()
    var quit = false

    val mainMenu = """     
        -----------------------------------
        1. Hitta konton för en given kund
        2. Sök efter kunder på (del av) namn
        3. Sätt in pengar
        4. Ta ut pengar
        5. Överför pengar mellan konton
        6. Skapa nytt konto
        7. Radera existerande konto
        8. Skriv ut alla konton i banken
        9. Skriv ut ändringshistoriken
        10. Återställ banken till ett tidigare datum
        11. Avsluta
        """

    def main(args: Array[String]): Unit = {
        println("Bankapplication starting...")
        
        keepState
        println(s"bankfind : ${bank.findByNumber( 1001).get} ")
        while (!quit) {
            println(mainMenu)
            val input = readInt("Val: ")
            handleInput(input)
            println(Date.now().toNaturalFormat)
        }
        
    }

    def keepState: Unit = {
        val allEntries = Mechanic.readFile
        allEntries.foreach(x => bank.doEvent(x.event, false, x.date))
        println(s"Keepstate allentries: $allEntries")
    }


    def handleInput(in: Int): Unit = {
        in match {
            case 1 => {
                bank.findAccountsForHolder(readInt("Id: ").toLong).foreach(printAccount)
            }
            case 2 => {
                bank.findByName(readString("Namn: ")).map(x => println(x.toString()))
            }
            case 3 => {
                val acc = readInt("Kontonummer: ")
                val sum = readInt("Summa: ")
                println(bank.doEvent(Deposit(acc, sum), true))
            }
            case 4 => { // Kolla ej går fel 
                val acc = readInt("Kontonummer: ")
                val sum = readInt("Summa: ")
                println(bank.doEvent(Withdraw(acc, BigInt(sum)), true))
            }
            case 5 => { // Kolla ej går fel 
                val from = readInt("Kontonummer att överföra ifrån: ")
                val to = readInt("Kontonummer att överföra till: ")
                val sum = readInt("Summa. ")
                println(bank.doEvent(Transfer(from, to, sum), true))
            }
            case 6 => { 
                val name = readLine("Namn: ")
                val id = readId

                println(bank.doEvent(NewAccount(id, name), true))
                println(BankAccount.getHead)
            }
            case 7 => { // Kolla ej går fel
                val target = readInt("Kontonummer att förgöra: ")
                println(bank.doEvent(DeleteAccount(target), true))
                // hur gör jag för att kolla om kontot inte finns
            }
            case 8 => {
                bank.allAccounts().sortBy(_.holder.name.toLowerCase()).foreach(x => printAccount(x))
            }
            case 9 => {
                bank.history().foreach(x => printEntry(x))
            }
            case 10 => {
                println("Vilket datum vill du återställa banken till?")
                val returnDate = Date(readInt("År: "), readInt("Månad: "), readInt("Datum(dag): "), readInt("Timme: "), readInt("Minut: "))
                println(bank.returnToState(returnDate))
            }
            case 11 => {
                quit = true 
            }
            case _ => 



            // att fixa
            // unika ID
            // bankkontonummer 
        }
    }
    
    @tailrec
    def readInt(x: String): Int = {
        
        try {
            val inp = readLine(x).toInt
            if (inp < 0) throw new IllegalStateException() else inp
        }
        catch {
            case _: IllegalStateException => {
                println("Du kan inte ta ut negativa pengar xddd")
                readInt(x)
            }
            case _: Throwable => {
                println("Write an integer bing bong")
                readInt(x)
            }
        }
    }
    
    @tailrec
    def readString(x: String): String = {
        try {
            readLine(x)
        }
        catch {
            case _: Throwable => {
                println("Write a string bing bong")
                readString(x)
            }
        }
    } 

    def readId: Long = {
        try {
            val id = readInt("Id: ").toLong
            if (bank.allAccounts().map(x => x.holder.id).contains(id)) throw new IllegalArgumentException else id
        } catch {
            case _: IllegalArgumentException => {
                println("Tyvärr, detta ID finns redan, skriv in ett annat")
                readId
            }
        }
    }

    def printAccount(acc: BankAccount): Unit = {
        println(s"Konto ${acc.accountNumber} (${acc.holder.name}, id: ${acc.holder.id}) ${acc.getBalance} kr")
    }

    def printEntry(h: HistoryEntry): Unit = {
        println(s"${h.date.toNaturalFormat}: ${h.toNaturalFormat} ")
    }

}