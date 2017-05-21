module Todaybot.Morph

-- QUESTION/DISCUSSION
-- should this be in std library?                                          
-- there's maybeToEither in there...                                       
-- although I don't particularly like removing the                         
-- exception info here.                                                    
-- In general, I think todaybot should have an exception
-- style which propagates rich exceptions somehow rather than
-- hiding them like this.
public export eitherToMaybe : Either e v -> Maybe v
eitherToMaybe (Left err) = Nothing
eitherToMaybe (Right v) = Just v

