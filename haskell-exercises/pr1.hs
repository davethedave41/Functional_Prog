module Practice1 where 
    
headx ::  [a] ->  a
headx (x:_) =  x
headx [] = error "Empty list"

headm :: [a] -> Maybe a
headm (x:_) = Just x
headm [] = Nothing

heade :: [a] -> Either String a
heade (x:_) = Right x
heade [] = Left "Not full anyways"

{- The Either type represents values with two possibilities:
     a value of type Either a b is either Left a or Right b.
     The Either type is sometimes used to represent a value which is either 
     correct or an error; by convention, the Left constructor is used to hold 
     an error value and the Right constructor is used to hold a correct 
  value (mnemonic: "right" also means "correct").
-}

{- An instance of the monad class for the Maybe type:
    instance Monad Maybe where
        return a = Just a
        Nothing >>= f = Nothing
        (Just x) >>= f = f x
        fail s = Nothing
-}

headmon :: Monad m => [a] -> m a
headmon (x:_) = return x
headmon [] = fail "Loser"

{-(>>=) :: forall a b. m a -> (a -> m b) -> m b     infixl 1

  Sequentially compose two actions, passing any value produced
   by the first as an argument to the second.
  
  (>>) :: forall a b. m a -> m b -> m b     infixl 1
  
  Sequentially compose two actions, discarding any 
   value produced by the first, like sequencing
   operators (such as the semicolon) in imperative languages.
  
  return :: a -> m a
  
  Inject a value into the monadic type.
  
  fail :: String -> m a
  
  Fail with a message. This operation is not part of the mathematical
   definition of a monad, but is invoked on pattern-match
   failure in a do expression.
-}   