import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

data Expr = Val Integer | Var String | Add Expr Expr

type Env = M.Map String Integer
type Eval a = ExceptT String (State Env) a

eval :: Expr -> Eval Integer
eval (Val n)   = return n
eval (Var x)   = do
  env <- lift get
  case M.lookup x env of
    Just v  -> return v
    Nothing -> throwError "Error"
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2

runEval :: Expr -> Env -> Either String Integer
runEval expr env = evalState (runExceptT (eval expr)) env

main :: IO ()
main = do
  let env  = M.fromList [("x", 100), ("y", 23)]
      ast  = Add (Var "x") (Var "y")
  case runEval ast env of
    Right val -> print val
    Left _    -> putStrLn "0"
