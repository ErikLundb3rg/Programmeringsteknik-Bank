package bank

//import java.nio.file._
//import java.nio.charset.StandardCharsets.UTF_8
//import javax.xml.crypto.Data
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import java.io.File
import java.io.PrintWriter
import java.io.FileWriter
import scala.io.Source

object Mechanic {
    val fileName = "bank_log.txt"

    def writeFile(event: HistoryEntry): Unit = {
        val writer = new PrintWriter(new FileWriter(fileName, true))
        writer.println(event.toLogFormat)
        writer.close()
        
    }

    def readFile: Vector[HistoryEntry] = {
        var xs = Vector[HistoryEntry]()
        val src = Source.fromFile(fileName)
        for (line <- src.getLines()) 
            xs = xs :+ HistoryEntry.fromLogFormat(line)
        xs
    }
    
    def eraseFile: Unit = {
        val writer = new PrintWriter(fileName);
        writer.print("");
        writer.close();
    }
}