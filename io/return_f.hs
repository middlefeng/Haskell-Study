





main = do
		  -- return returns IO Integer, <- extract the Integer part,
		  -- so one is Integer
          one <- return 1

          let two = 2
          putStrLn (show (one + two))
