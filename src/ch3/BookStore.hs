data BookInfo =
  Book Int
       String
       [String]
  deriving (Show)

data BookReview =
  BookReview BookInfo
             CustomerID
             String

type CustomerID = Int

type ReviewBody = String

data BetterReview =
  BetterReview BookInfo
               CustomerID
               ReviewBody

type BookRecord = (BookInfo, BookReview)

data Customer = Customer
  { customerID :: CustomerID
  , customerName :: String
  } deriving (Show)