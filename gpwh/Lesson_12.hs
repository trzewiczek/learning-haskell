module Lesson_12 where

type FirstName = String
type MiddleName = String
type LastName = String
type PatientName = (FirstName, LastName)

firstName :: PatientName -> String
firstName = fst

lastName :: PatientName -> String
lastName = snd

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

type Age = Int
type Height = Int

data Sex = 
    Male
  | Female

data RhType = Pos | Neg
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

data ABOType = A | B | AB | O
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

data BloodType = BloodType ABOType RhType
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data Patient = Patient { name :: Name 
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

johnDoe :: Patient
johnDoe = Patient { name = Name "John" "Doe"
                  , age = 43
                  , sex = Male
                  , height = 62
                  , weight = 115
                  , bloodType = BloodType O Neg }

janeDoe :: Patient
janeDoe = Patient { name = NameWithMiddle "Jane" "Elisabeth" "Doe"
                  , age = 23
                  , sex = Female
                  , height = 45
                  , weight = 90
                  , bloodType = BloodType A Pos }
                  
-- Q12.1
canDonateTo :: Patient -> Patient -> Bool
canDonateTo p1 p2 = let canDonateBloodTo (BloodType O _) _ = True
                        canDonateBloodTo _ (BloodType AB _) = True
                        canDonateBloodTo (BloodType A _) (BloodType A _) = True
                        canDonateBloodTo (BloodType B _) (BloodType B _) = True
                        canDonateBloodTo _ _ = False
                    in canDonateBloodTo (bloodType p1) (bloodType p2)

-- Q12.2
patientSummary :: Patient -> String
patientSummary p = let frame = "**************"
                       nameStr (Name f l) = "Patient Name: " ++ l ++ ", " ++ f
                       nameStr (NameWithMiddle f m l) = "Patient Name: " ++ l ++ ", " ++ f ++ " " ++ m
                       sexStr Male = "Sex: Male"
                       sexStr Female = "Sex: Female"
                       ageStr a = "Age: " ++ show a
                       heightStr h = "Height: " ++ show h
                       weightStr w = "Weight: " ++ show w
                       bloodStr b = "Blood Type: " ++ showBloodType b
                   in frame ++ "\n" ++
                      nameStr (name p) ++ "\n" ++
                      sexStr (sex p) ++ "\n" ++ 
                      ageStr (age p) ++ "\n" ++ 
                      heightStr (height p) ++ "\n" ++ 
                      weightStr (weight p) ++ "\n" ++ 
                      bloodStr (bloodType p) ++ "\n" ++ 
                      frame ++ "\n"
