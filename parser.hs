import Prelude
import System.Environment ( getArgs )
import Data.Maybe

main = do
    args <- getArgs
    let expr = generate_expression args
    print expr
    let value = eval expr
    print_value value

-- definitions

data Operator = OpPlus | OpMinus | OpMult | OpDivide deriving (Show)
data Value 
          = 
          None { }
          | 
          Vstr { s_val :: String }
          |
          Vnum { n_val :: Float } deriving (Show)

data Expr = 
          Empty { }
          | 
          Eop { op :: Operator, left :: Expr, right :: Expr }
          | 
          Estr { str_val :: String }
          |
          Enum { num_val :: Float } deriving (Show)

data Symbol = Symbol { expr :: Expr, success :: Bool }
-- helpers

isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

make_expr :: String -> Expr
make_expr "+" =  Eop OpPlus Empty Empty
make_expr "x" = Eop OpMult Empty Empty
make_expr "-" = Eop OpMinus Empty Empty
make_expr "/" = Eop OpDivide Empty Empty
make_expr a = if isNumber a then Enum(read a) else Estr(a)

print_value :: Value -> IO ()
print_value v =
    case v of
        None -> print("empty result")
        Vnum n -> print(n)
        Vstr s -> print(s)

-- functions

generate_expression :: [String] -> Expr
generate_expression [null] = Empty
generate_expression xs = 
    do
        foldl (\acc x -> symbolize acc x) (Empty) xs

symbolize :: Expr -> String -> Expr
symbolize a b =
    do
        let sym = symbolize_helper a b
        case sym of
            Symbol e _ -> e


symbolize_helper :: Expr -> String -> Symbol 
-- currently does not work on nested expressions like 
    -- + + 1 1 + 1 1
-- but it does work on things like 
    -- + 3 x 4 4


symbolize_helper (Enum a) b = Symbol Empty False
symbolize_helper (Estr a) b = Symbol Empty False
symbolize_helper (Eop op Empty r) b = Symbol (Eop op (make_expr b) r) True
symbolize_helper (Eop op (Eop l_op l_l l_r) Empty) b = 
    do
        let l_sym = symbolize_helper (Eop l_op l_l l_r) b
        case l_sym of 
            Symbol l_expr success -> if True then Symbol (Eop op l_expr Empty) True else Symbol Empty False

symbolize_helper (Eop op l Empty) b = Symbol (Eop op l (make_expr b)) True
symbolize_helper (Eop op l (Eop r_op r_l r_r)) b =
    do 
        let r_sym = symbolize_helper (Eop r_op r_l r_r) b
        case r_sym of
            Symbol r_expr success -> if True then Symbol (Eop op l r_expr) True else Symbol Empty False

symbolize_helper (Empty) b = do
    let b_expr = make_expr b
    Symbol b_expr True

eval :: (Expr) -> (Value)
eval a = 
    case a of 
        Empty -> None
        Enum n -> Vnum(n)
        Estr s -> Vstr(s)
        Eop op l r ->
            do
                let l_val = eval l
                let r_val = eval r
                case op of
                    OpPlus -> 
                        do
                            case l_val of
                                Vnum l_v -> 
                                    case r_val of
                                        Vnum r_v-> 
                                            do
                                                let sum = l_v + r_v
                                                Vnum(sum)
                                        _ -> None
                                _ -> None
                    OpMinus -> 
                        do
                            case l_val of
                                Vnum l_v -> 
                                    case r_val of
                                        Vnum r_v-> 
                                            do
                                                let sum = l_v - r_v
                                                Vnum(sum)
                                        _ -> None
                                _ -> None
                    OpMult -> 
                        do
                            case l_val of
                                Vnum l_v -> 
                                    case r_val of
                                        Vnum r_v -> 
                                            do
                                                let product = l_v * r_v
                                                Vnum(product)
                                        _ -> None
                                _ -> None
                    OpDivide -> 
                        do
                            case l_val of
                                Vnum l_v -> 
                                    case r_val of
                                        Vnum r_v -> 
                                            do
                                                let product = l_v / r_v
                                                Vnum(product)
                                        _ -> None
                                _ -> None            