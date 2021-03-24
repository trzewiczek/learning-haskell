-- Q1.1

-- Prelude> 2^123
-- 10633823966279326983230456482242756608


-- Q1.2

toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"

fromPart author = "Yours, " ++ author ++ "."

createEmail recipient title author = toPart recipient ++
                                    bodyPart title ++
                                    fromPart author

email = do
  print "Who is this for?"
  recipient <- getLine
  
  print "What is the title?"
  title <- getLine 

  print "Who's the author?"
  author <- getLine 

  print (createEmail recipient title author)

main = email