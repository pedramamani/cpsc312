
{-# LANGUAGE InstanceSigs #-}
module Lib where

todo :: a
todo = undefined

type VarId = Integer

type Expr u a = u (Either VarId a)

class Unifiable u where 
    unify :: (Eq a) => Substitution u a -> Expr u a -> Expr u a -> [Substitution u a]

type Substitution u a = [(VarId, Expr u a)]

data PExpr a = Value a | Term String [PExpr a]
    deriving (Show, Eq, Ord)

instance Monad PExpr where
    return :: a -> PExpr a
    return a = Value a

    (>>=) :: PExpr a -> (a -> PExpr b) -> PExpr b
    (Value a) >>= f = f a
    (Term s pexprs) >>= f = Term s (map (>>= f) pexprs)

instance Functor PExpr where
    fmap :: (a -> b) -> PExpr a -> PExpr b
    fmap f pa = pa >>= return . f

instance Applicative PExpr where
    pure :: a -> PExpr a
    pure = return

    (<*>) :: PExpr (a -> b) -> PExpr a -> PExpr b
    pf <*> pa = pf >>= (\f -> pa >>= return . f)

-- | Turn a variable into its corresponding expression.
pureVarId :: Monad m => VarId -> Expr m a
pureVarId = return . Left

-- | Turn a simple value into its corresponding expression.
pureA :: Monad m => a -> Expr m a
pureA = return . Right

one, two, three, four :: Expr PExpr String
one = pureA "1"
two = pureA "2"
three = pureA "3"
four = pureA "4"

x, y, z, xs, ys, zs, empty :: Expr PExpr a
x = pureVarId 1
y = pureVarId 2
z = pureVarId 3
xs = pureVarId 4
ys = pureVarId 5
zs = pureVarId 6
empty = Term "[]" []

cons :: Expr PExpr a -> Expr PExpr a -> Expr PExpr a
cons a as = Term "|" [a, as]

list :: [Expr PExpr a] -> Expr PExpr a
list = foldr cons empty

l12, l34, l1234 :: Expr PExpr String
l12 = cons one (cons two empty)
l34 = cons three (cons four empty)
l1234 = list (map pureA ["1", "2", "3", "4"])

-- | For each variable in s, walk replaces it anywhere it appears in
-- expr with its value. It then continues these replacements until there
-- are no more to do.
--
-- Note that we really only have >>= available to operate on expr
-- because all we actually know about it is that it's a Monad!
--
-- As examples, nothing happens when we walk with an empty substitution:
--
-- >>> walk [] (Term "Hello" [Value (Right "World!")])
-- Term "Hello" [Value (Right "World!")]
--
-- Just substituting out a variable gives us that variable's value:
--
-- >>> walk [(7, Value (Left 3))] (Value (Left 7))
-- Value (Left 3)
--
-- But walk will do all the substitutions it finds anywhere inside
-- an expression, even if those create more substitutions to perform in turn:
--
-- >>> walk [(1, Value (Left 2)), (2, Value (Right "Two")), (3, Term "Three" [Value (Left 1), Value (Left 4)])]
-- Term "Three" [Value (Right "Two"),Value (Left 4)]

walk :: (Monad m, Eq a) => Substitution m a -> Expr m a -> Expr m a
walk s expr = expr >>= (either tryS pureA)
    where tryS v = case lookup v s of
                       Nothing -> pureVarId v
                       Just e -> walk s e

extendSub :: Substitution u a -> VarId -> Expr u a -> Substitution u a
extendSub s v e = (v, e):s

emptySub :: Substitution u a
emptySub = []

instance Unifiable PExpr where
    unify :: (Eq a) => Substitution PExpr a -> Expr PExpr a -> Expr PExpr a -> [Substitution PExpr a]
    unify s u v = helper (walk s u) (walk s v)
        where
            helper (Value (Left v1)) (Value (Left v2)) | v1 == v2 = [s]

            helper (Value (Left v)) e = [extendSub s v e]
            helper e (Value (Left v)) = [extendSub s v e]

            helper (Value (Right l)) (Value (Right r)) | l == r = [s]
                                                       | otherwise = []

            helper (Term _ _) (Value (Right _)) = []
            helper (Value (Right _)) (Term _ _) = []

            helper (Term s1 us) (Term s2 vs) | s1 /= s2 || (length us /= length vs) = []
                                             | otherwise = unifyAll [s] us vs

            unifyAll ss [] [] = ss
            unifyAll ss (u':us) (v':vs) = unifyAll (concat $ map (\s -> unify s u' v') ss) us vs
            unifyAll _ [] _ = error "Only use with equal length lists."
            unifyAll _ _ [] = error "Only use with equal length lists."

instance Unifiable [] where
    unify :: (Eq a) => Substitution [] a -> Expr [] a -> Expr [] a -> [Substitution [] a]
    unify s u v = helper (walk s u) (walk s v)
        where
            helper [] [] = [s]

            helper (Right a : as) [] = []
            helper [] (Right a : as) = []

            helper (Right a : as) (Right b : bs) | a /= b = []
                                                 | otherwise = unify s as bs

            helper [Left a] [Left b] | a == b = [s]

            helper (Left n:xs) ys = concatMap applyChoice (splits ys)
                where applyChoice (start, rest) | elem (Left n) start = []
                                                | otherwise = unify (extendSub s n start) xs rest

            helper ys (Left n:xs) = helper (Left n:xs) ys

-- | splits xs returns a list of tuples (left, right)
-- such that left ++ right == xs. The lists come back in
-- order of increasing length of left, beginning with ([], xs).
splits :: [a] -> [([a],[a])]
splits as = map (flip splitAt as) [0..(length as)]
