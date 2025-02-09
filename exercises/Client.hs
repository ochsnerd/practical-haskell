module Client where

data Client id
  = GovOrg {clientId :: id, clientName :: String}
  | Company
      { clientId :: id,
        clientName :: String,
        person :: Person,
        duty :: String
      }
  | Individual {clientId :: id, person :: Person}
  deriving (Show, Eq, Ord)

data Person = Person {firstName :: String, lastName :: String}
  deriving (Show, Eq, Ord)

clients :: [Client Int]
clients = [GovOrg 1 "hello", GovOrg 99 "abc", Company 2 "blabla" (Person "John" "Dev") "big boss I wish", Individual 3 (Person "John" "Dev also?")]
