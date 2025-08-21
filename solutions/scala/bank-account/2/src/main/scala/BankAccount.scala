trait BankAccount:
   def closeAccount(): Unit
   def getBalance: Option[Int]
   def incrementBalance(increment: Int): Option[Int]

object Bank:
   def openAccount(): BankAccount = new BankAccount:
      private var (bal, isOpen): (Int, Boolean) = (0, true)

      override def closeAccount(): Unit = this.synchronized {
         isOpen = false
      }

      override def getBalance: Option[Int] = this.synchronized {
         if isOpen then Some(bal) else None
      }

      override def incrementBalance(increment: Int): Option[Int] = this.synchronized {
         if isOpen then
            bal += increment
            Some(bal)
         else None
      }
