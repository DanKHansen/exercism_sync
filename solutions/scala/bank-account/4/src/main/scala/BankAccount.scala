trait BankAccount:
   def closeAccount(): Unit
   def getBalance: Option[Int]
   def incrementBalance(increment: Int): Option[Int]

object Bank:
   def openAccount(): BankAccount = new BankAccount():
      private var bal: Option[Int] = Some(0)
      override def closeAccount(): Unit = bal = None

      override def getBalance: Option[Int] = bal

      override def incrementBalance(increment: Int): Option[Int] = synchronized:
         bal = bal.map(_ + increment)
         bal
