package bank


case class Customer(name: String, id: Long) {
    override def toString(): String = s"Customer name: $name, id: $id"
    
}