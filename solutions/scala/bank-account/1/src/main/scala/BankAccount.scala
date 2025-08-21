trait BankAccount:
   def closeAccount(): Unit
   def getBalance: Option[Int]
   def incrementBalance(increment: Int): Option[Int]

object Bank:
   def openAccount(): BankAccount = new BankAccount:
      private var bal: Int = 0
      private var isOpen: Boolean = true
      private val lock = new Object

      override def closeAccount(): Unit = lock.synchronized {
         isOpen = false
      }

      override def getBalance: Option[Int] = lock.synchronized {
         if isOpen then Some(bal) else None
      }

      override def incrementBalance(increment: Int): Option[Int] = lock.synchronized {
         if isOpen then
            bal += increment
            Some(bal)
         else None
      }
