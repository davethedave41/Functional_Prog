{- urdaibad David Urdaibay -}
module Ex02 where
  import Data.List ((\\))
  import Data.Maybe
  
  -- Datatypes -------------------------------------------------------------------
  
  -- do not change anything in this section !
  
  type Id = String
  
  data Expr
    = Val Double
    | Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Dvd Expr Expr
    | Var Id
    | Def Id Expr Expr
    deriving (Eq, Show)
  
  type Dict k d  =  [(k,d)]
  
  define :: Dict k d -> k -> d -> Dict k d
  define d s v = (s,v):d
  
  find :: Eq k => Dict k d -> k -> Maybe d
  find []             _                 =  Nothing
  find ( (s,v) : ds ) name | name == s  =  Just v
                           | otherwise  =  find ds name
  
  type EDict = Dict String Double
  
  v42 = Val 42 ; j42 = Just v42
  
  -- Part 1 : Evaluating Expressions -- (60 test marks, worth 15 Exercise Marks) -
  
  -- Implement the following function so all 'eval' tests pass.
  
  -- eval should return Nothing if:
    -- (1) a divide by zero operation was going to be performed;
    -- (2) the expression contains a variable not in the dictionary.
  
  eval :: EDict -> Expr -> Maybe Double
  eval _ (Val x) = Just x
  eval ed (Var i) = find ed i
  eval ed (Add exp1 exp2) = case (eval ed exp1, eval ed exp2) of      -- Use of case condition to be able to return output of type 'Maybe'
                                 (Just exp1, Just exp2) -> Just(exp1 + exp2)
                                 _                      -> Nothing    -- if the expression is not in the dictionary then nothing is returned
  eval ed (Sub exp1 exp2) = case (eval ed exp1, eval ed exp2) of
                                 (Just exp1, Just exp2) -> Just(exp1 - exp2)
                                 _                      -> Nothing
  eval ed (Mul exp1 exp2) = case (eval ed exp1, eval ed exp2) of
                                 (Just 0, _)            -> Just 0
                                 (_, Just 0)            -> Just 0
                                 (Just exp1, Just exp2) -> Just(exp1 * exp2)
                                 _                           -> Nothing
  eval ed (Dvd exp1 exp2) = case (eval ed exp1, eval ed exp2) of
                                 (Just 0, _)            -> Nothing
                                 (_, Just 0)            -> Nothing
                                 (Just exp1, Just exp2) -> Just(exp1 / exp2)
                                 _                      -> Nothing
  eval ed (Def i exp1 exp2) = case (eval ed exp1) of  -- add first expression into dictionary
                                   Just newVar -> eval (define ed i newVar) exp2  -- use new dictionary to evaluate this expression
                                   _           -> Nothing
  -- Part 1 : Expression Laws -- (15 test marks, worth 15 Exercise Marks) --------
  
  {-
  
  There are many, many laws of algebra that apply to our expressions, e.g.,
  
    x + y            =  y + z         Law 1
    x + (y + z)      =  (x + y) + z   Law 2
    x - (y + z)      =  (x - y) - z   Law 3
    (x + y)*(x - y)  =  x*x - y*y     Law 4
    ...
  
    We can implement these directly in Haskell using Expr
  
    Function LawN takes an expression:
      If it matches the "shape" of the law lefthand-side,
      it replaces it with the corresponding righthand "shape".
      If it does not match, it returns Nothing
  
      Implement Laws 1 through 4 above
  -}
  
  
  law1 :: Expr -> Maybe Expr
  law1 e = case e of
                (Add exp1 exp2) -> Just(Add exp2 exp1)
                _               -> Nothing
  
  law2 :: Expr -> Maybe Expr
  law2 e = case e of 
                (Add exp1 (Add exp2 exp3)) -> Just(Add (Add exp1 exp2) exp3)
                _                          -> Nothing
  
  law3 :: Expr -> Maybe Expr
  law3 e = case e of
                (Sub exp1 (Add exp2 exp3)) -> Just(Sub (Sub exp1 exp2) exp3)
                _                          -> Nothing

  law4 :: Expr -> Maybe Expr
  law4 e = case e of
                (Mul (Add exp1 exp2) (Sub exp3 exp4)) -> Just(Sub (Mul exp1 exp1) (Mul exp2 exp2)) 
                _                                     -> Nothing
               

  