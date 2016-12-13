module CNF where

import           Data.List        (intercalate)
import           Prelude   hiding (exp)
import           Ordinal          (Ordinal(..), OrdinalComparison(..))

type CNFSummand = (CNF, Int)

data CNF = CNF [CNFSummand] Int

instance Eq CNF where
    (CNF [] xn)            == (CNF [] yn)            = xn == yn
    (CNF [] xn)            == _                      = False
    _                      == (CNF [] yn)            = False
    (CNF ((xe, xm):xs) xn) == (CNF ((ye, ym):ys) yn) = xe == ye && xm == ym && (CNF xs xn) == (CNF ys yn)

instance Ord CNF where
    compare (CNF [] xn)            (CNF [] yn)                      = compare xn yn
    compare (CNF [] _)             _                                = LT
    compare _                      (CNF [] _)                       = GT
    compare (CNF ((xe, xm):xs) xn) (CNF ((ye, ym):ys) yn)
                                                        | xe /= ye  = compare xe ye
                                                        | xm /= ym  = compare xm ym
                                                        | otherwise = compare (CNF xs xn) (CNF ys yn)

instance Show CNF where
    show cnf@(CNF s _) = (intercalate " + " $ map showSummand s) ++ (showFreeCoeff cnf)
      where
        showSummand (e, m) = "w" ++ showExp e ++ showCoeff m
        showExp (CNF [] 1) = ""
        showExp (CNF [] n) = " ^ " ++ show n
        showExp e          = " ^ (" ++ show e ++ ")"
        showCoeff 1 = ""
        showCoeff m = " * " ++ show m
        showFreeCoeff (CNF [] n) = show n
        showFreeCoeff (CNF _ 0)  = ""
        showFreeCoeff (CNF _ n)  = " + " ++ show n

ordinalToCNF :: Ordinal -> CNF
ordinalToCNF (Atom n)  = CNF [] n
ordinalToCNF Omega     = CNF [(CNF [] 1, 1)] 0
ordinalToCNF (Sum x y) = add (ordinalToCNF x) (ordinalToCNF y)
ordinalToCNF (Mul x y) = mul (ordinalToCNF x) (ordinalToCNF y)
ordinalToCNF (Exp x y) = exp (ordinalToCNF x) (ordinalToCNF y)

add :: CNF -> CNF -> CNF
add (CNF [] xn) (CNF [] yn) = CNF [] $ xn + yn
add (CNF xs xn) (CNF [] yn) = CNF xs $ xn + yn
add (CNF [] _) y            = y
add x y                     = case compare (fe x) (fe y) of
                              LT -> y
                              EQ -> appendFirst (fe x, fc x + fc y) $ cnfTail y
                              GT -> appendFirst (cnfHead x) $ add (cnfTail x) y

mul :: CNF -> CNF -> CNF
mul (CNF [] 0) _            = CNF [] 0
mul _ (CNF [] 0)            = CNF [] 0
mul (CNF [] xn) (CNF ys yn) = CNF ys $ xn * yn
mul x y                     = mulInfin x y
  where
    mulInfin x (CNF [] 0)             = CNF [] 0
    mulInfin x (CNF [] yn)            = appendFirst (fe x, fc x * yn) $ cnfTail x
    mulInfin x (CNF ((ye, ym):ys) yn) = appendFirst (add (fe x) ye, ym) $ mulInfin x (CNF ys yn)

exp :: CNF -> CNF -> CNF
exp _ (CNF [] 0)            = CNF [] 1
exp (CNF [] 0) _            = CNF [] 0
exp (CNF [] xn) (CNF [] yn) = CNF [] $ xn ^ yn
exp x (CNF [] yn)           = expFin x yn
  where
    expFin x 1  = x
    expFin x yn = mul x $ expFin x $ yn - 1
exp (CNF [] xn) (CNF ys yn) = mul (CNF [(CNF ys 0, 1)] 0) $ CNF [] $ xn * yn
exp x (CNF ys yn)           = mul (CNF [(mul (fe x) $ CNF ys 0, 1)] 0) $ exp x $ CNF [] yn

fe :: CNF -> CNF
fe (CNF [] n)         = CNF [] 0
fe (CNF ((e, _):_) _) = e

fc :: CNF -> Int
fc (CNF [] n)         = n
fc (CNF ((_, m):_) _) = m

cnfHead :: CNF -> CNFSummand
cnfHead (CNF [] n)     = error "head of finite doesn't exist"
cnfHead (CNF (x0:_) _) = x0

cnfTail :: CNF -> CNF
cnfTail (CNF [] n)    = error "tail of finite doesn't exist"
cnfTail (CNF (_:s) n) = CNF s n

appendFirst :: CNFSummand -> CNF -> CNF
appendFirst x (CNF ys yn) = CNF (x:ys) yn