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

-- is this in std library? should it be?
-- (waffle: maybeHead is a natural transformation but
-- not a monad homomorphism...)
-- Goes alongside eitherToMaybe as a morphism and
-- maybe should be called listToMaybe to line up
-- with that?
public export maybeHead : List a -> Maybe a
maybeHead [] = Nothing
maybeHead (x::xs) = Just x

-- QUESTION/DISCUSSION: Replace uses of this by binding
-- inside Maybe instead of extracting.
public export partial fromJust : Maybe a -> a
fromJust (Just v) = v

