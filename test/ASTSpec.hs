{-# LANGUAGE PolyKinds, QuantifiedConstraints, FlexibleInstances, PatternSynonyms, RankNTypes, DataKinds, KindSignatures, TypeFamilies, PolyKinds, InstanceSigs, ScopedTypeVariables, TypeApplications, MultiParamTypeClasses, LambdaCase, GADTs #-}

module ASTSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Char
import Data.Tree
import Data.IFunctor.Foldable
import Text.Read
import Data.Singletons
import Data.Functor.Classes
import Data.Kind (Type)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    -- describe "Anamorphisms work" $ do
        -- it "Parse (+ 1 2)" $
            -- parseExp (Node "+" [Node "1" [], Node "2" []])
            -- `shouldBe`
            -- Right (EFac (FAdd (EInt 1) (EInt 2)))
        -- it "Parse (+ (- 1) 3)" $
            -- parseExp (Node "+" [Node "-" [Node "1" []], Node "3" []])
            -- `shouldBe`
            -- Right (EFac (FAdd (EFac (FNeg (EInt 1))) (EInt 3)))
    describe "Paramorphisms work" $ do
        it "Eval (+ 1 2)" $
            evalExp (EFac (FAdd (EInt 1) (EInt 2)))
            `shouldBe`
            3
        it "Eval (+ (- 1) 3)" $
            evalExp (EFac (FAdd (EFac (FNeg (EInt 1))) (EInt 3)))
            `shouldBe`
            2
    -- describe "Hylomorphisms work" $ do
        -- it "ParseEval (+ 1 2)" $
            -- parseEval (Node "+" [Node "1" [], Node "2" []])
            -- `shouldBe`
            -- 3
        -- it "ParseEval (+ (* 1 2) 3)" $
            -- parseEval (Node "+" [Node "*" [Node "1" [], Node "2" []], Node "3" []])
            -- `shouldBe`
            -- 5


-- * Implementations

data ASTIndex = Exp | Fac
data SAST ix where
    SExp :: SAST 'Exp
    SFac :: SAST 'Fac
type instance Sing = SAST

data AST f ix where
    EIntF :: Int -> AST f 'Exp
    EFacF :: f 'Fac -> AST f 'Exp
    FNegF :: f 'Exp -> AST f 'Fac
    FAddF :: f 'Exp -> f 'Exp -> AST f 'Fac

pattern EInt :: Int -> IFix AST Exp
pattern EInt n = IFix (EIntF n)
pattern EFac :: IFix AST Fac -> IFix AST Exp
pattern EFac x = IFix (EFacF x)
pattern FNeg :: IFix AST Exp -> IFix AST Fac
pattern FNeg a = IFix (FNegF a)
pattern FAdd :: IFix AST Exp -> IFix AST Exp -> IFix AST Fac
pattern FAdd a b = IFix (FAddF a b)

-- instance IShow AST where
    -- ishowsPrec p (EIntF n)   = showParen (p > 10) $ showString "EIntF " . showsPrec 11 n
    -- ishowsPrec p (EFacF x)   = showParen (p > 10) $ showString "EFacF " . showsPrec 11 x
    -- ishowsPrec p (FNegF x)   = showParen (p > 10) $ showString "FNegF " . showsPrec 11 x
    -- ishowsPrec p (FAddF a b) = showParen (p > 10) $ showString "FAddF " . showsPrec 11 a . showString " " . showsPrec 11 b

-- instance IEq AST where
    -- ieq (EIntF n)   (EIntF m)   = n == m
    -- ieq (EFacF x)   (EFacF y)   = x == y
    -- ieq (FNegF x)   (FNegF y)   = x == y
    -- ieq (FAddF a b) (FAddF x y) = a == x && b == y
    -- ieq _ _ = False

-- ex' :: IFix AST ix -> IFix AST ix -> Bool
-- ex' = (==)

-- ex :: IFix AST ix -> String
-- ex = show

-- instance Show (IFix AST Exp) where
    -- show (EInt n) = show n
    -- show (EFac x) = show x
-- instance Show (IFix AST Fac) where
    -- show (FNeg x) = "(- " ++ show x ++ ")"
    -- show (FAdd a b) = "(+ " ++ show a ++ " " ++ show b ++ ")"

instance IFunctor AST where
    imap :: forall ix a b. SingI ix
         => (forall ix. SingI ix => a ix -> b ix)
         -> AST a ix -> AST b ix
    imap f =
        case sing @ix of
          SExp -> \case
            EIntF n -> EIntF n
            EFacF x -> EFacF (withSingI SFac (f x))
          SFac -> \case
            FNegF x -> FNegF (withSingI SExp (f x))
            FAddF a b -> FAddF (withSingI SExp (f a)) (withSingI SExp (f b))

-- * Testing

type EvalRes = Const Int

evalAlg :: forall ix. SingI ix => AST EvalRes ix -> EvalRes ix
evalAlg =
    case sing @ix of
      SExp -> \case
        EIntF n -> Const n
        EFacF (Const n) -> Const n
      SFac -> \case
        FNegF (Const n) -> Const (- n)
        FAddF (Const a) (Const b) -> Const (a + b)

data family ParseRes (f :: ASTIndex -> Type) (ix :: ASTIndex) :: Type
data instance ParseRes f Exp = PE { unPE :: Either String (AST f Exp) }
data instance ParseRes f Fac = PF { unPF :: Either String (AST f Fac) }
instance IFunctor ParseRes where
    imap :: forall ix a b. SingI ix
         => (forall ix. SingI ix => a ix -> b ix)
         -> ParseRes a ix -> ParseRes b ix
    imap f =
      case sing @ix of
        SExp -> \(PE x) -> PE $ fmap (imap f) x
        SFac -> \(PF x) -> PF $ fmap (imap f) x

parseCoalg :: forall ix. SingI ix => Const (Tree String) ix -> ParseRes (Const (Tree String)) ix
parseCoalg (Const s) =
  case sing @ix of
    SExp ->
      case s of
        Node s []
          | Just n <- readMaybe s -> PE $ Right $ EIntF n
          | otherwise -> PE $ Left $ "Error parsing \"" ++ s ++ "\" into an integer"
        _ -> PE $ Right $ EFacF (Const s)
    SFac ->
      case s of
        Node "+" [a, b] -> PF $ Right $ FAddF (Const a) (Const b)
        Node "+" c -> PF $ Left $ "Attempt to parse '+' with " ++ show (length c) ++ " children"
        Node "-" [a] -> PF $ Right $ FNegF (Const a)
        Node "-" c -> PF $ Left $ "Attempt to parse '-' with " ++ show (length c) ++ " children"
        Node x _ -> PF $ Left $ "Attempt to parse '" ++ x ++ "'"

liftErrors :: forall ix. SingI ix => IFix ParseRes ix -> Sum (Const String) (IFix AST) ix
liftErrors =
  case sing @ix of
    SExp -> \(IFix (PE x)) -> case x of
                                Left err -> InL $ Const err
                                -- Right ast -> InR $ IFix $ imap liftErrors ast

evalExp :: IFix AST Exp -> Int
evalExp e = getConst $ withSingI SExp $ cata evalAlg e

parseExp :: Tree String -> IFix ParseRes Exp
parseExp t = withSingI SExp $ ana parseCoalg $ Const t

-- parseEvalExp :: SingI ix
             -- => Const (Tree String) ix
             -- -> Const Int ix
-- parseEvalExp s = hylo evalAlg parseCoalg
