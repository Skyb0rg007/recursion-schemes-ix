{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module GRINSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Singletons.TH (singletons)
import Data.IFunctor.Foldable
import Text.Show (showListWith)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State

$(singletons [d|
    data ASTIdx = Exp | SimpleExp | Alt | Program | Def
    |])

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "lorem ipsum" $ do
        it "asdf" $
            1 `shouldBe` 1
    describe "Show instance for IFix" $ do
        it "is correct" $
            show (IFix $ ProgramF [] [IFix $ DefF "foo" [] $ IFix $ ESimpleF $ IFix $ SReturnF $ VSimple $ SVVar "x"])
            `shouldBe`
            "IFix (ProgramF [] [IFix (DefF \"foo\" [] (IFix (ESimpleF (IFix (SReturnF (VSimple (SVVar \"x\")))))))])"

type Name = String

data External = External
    { externalName :: Name
    , externalEffectful :: Bool
    -- other fields elided
    }
    deriving (Show, Read, Eq, Ord)

data Lit
    = LInt Int
    | LBool Bool
    deriving (Show, Read, Eq, Ord)

data Val
    = VTag Name [SimpleVal]
    | VSimple SimpleVal
    deriving (Show, Read, Eq, Ord)

data SimpleVal
    = SVLit Lit
    | SVVar Name
    deriving (Show, Read, Eq, Ord)

data LPat
    = LPTag Name [Name]
    | LPUnit
    deriving (Show, Read, Eq, Ord)

data CPat
    = CPTag Name [Name]
    | CPLit Lit
    | CPDefault
    deriving (Show, Read, Eq, Ord)

{- Original GRIN datatype

data 'Exp
  = Program     [External] [Def]
  | Def         Name [Name] 'Exp
  -- 'Exp
  | EBind       SimpleExp LPat 'Exp
  | ECase       Val [Alt]
  -- Simple 'Exp
  | SApp        Name [SimpleVal]
  | SReturn     Val
  | SStore      Val
  | SFetch      Name
  | SUpdate     Name Val
  | SBlock      'Exp
  -- Alt
  | Alt CPat 'Exp

type SimpleExp = 'Exp
type Alt = 'Exp
type Def = 'Exp
type Program = 'Exp

-}

{- What it really should be

data Program = Program [External] [Def]

data Def = Def Name [Name] 'Exp

data 'Exp
    = EBind SimpleExp LPat 'Exp
    | ECase Val [Alt]
    | ESimple SimpleExp

data SimpleExp
    = SApp Name [SimpleVal]
    | SReturn Val
    | SStore Val
    | SFetch Name
    | SUpdate Name Val
    -- | SBlock 'Exp

data Alt = Alt CPat 'Exp

-}

-- recursion-schemes-ix version

data AST (f :: ASTIdx -> *) (ix :: ASTIdx) where
    -- Program
    ProgramF :: [External] -> [f Def] -> AST f Program
    -- Def
    DefF :: Name -> [Name] -> f 'Exp -> AST f Def
    -- 'Exp
    EBindF :: f SimpleExp -> LPat -> f 'Exp -> AST f 'Exp
    ECaseF :: Val -> [f Alt] -> AST f 'Exp
    ESimpleF :: f SimpleExp -> AST f 'Exp
    -- SimpleExp
    SAppF :: Name -> [SimpleVal] -> AST f SimpleExp
    SReturnF :: Val -> AST f SimpleExp
    SStoreF :: Val -> AST f SimpleExp
    SFetchF :: Name -> AST f SimpleExp
    SUpdateF :: Name -> Val -> AST f SimpleExp
    -- Alt
    AltF :: CPat -> f 'Exp -> AST f Alt

type Exp = IFix AST 'Exp

instance IFunctor AST where
    imap = imapDefault

instance ITraversable AST where
    itraverse :: forall ix a b m. (Applicative m, SingI ix) => (forall ix. SingI ix => a ix -> m (b ix)) -> AST a ix -> m (AST b ix)
    itraverse f =
      case sing @ix of
        SProgram -> \case
          ProgramF externs defs -> ProgramF externs <$> (traverse (f @Def) defs)
        SDef -> \case
          DefF fn args body -> DefF fn args <$> f @'Exp body
        SExp -> \case
          EBindF e1 pat e2 -> EBindF <$> f @SimpleExp e1 <*> pure pat <*> f @'Exp e2
          ECaseF x alts -> ECaseF x <$> (traverse (f @Alt) alts)
          ESimpleF e -> ESimpleF <$> f @SimpleExp e
        SSimpleExp -> \case
          SAppF fn args -> pure $ SAppF fn args
          SReturnF x -> pure $ SReturnF x
          SStoreF x -> pure $ SStoreF x
          SFetchF l -> pure $ SFetchF l
          SUpdateF l x -> pure $ SUpdateF l x
        SAlt -> \case
          AltF cpat e -> AltF cpat <$> f @'Exp e

instance IShow AST where
    ishowsPrec :: forall ix a. SingI ix
               => (forall ix. SingI ix => Int -> a ix -> ShowS)
               -> Int -> AST a ix -> ShowS
    ishowsPrec sp p =
      case sing @ix of
        SProgram -> \case
          ProgramF externs defs -> showParen (p > 10) $
            showString "ProgramF " . showsPrec 11 externs . showString " " . showListWith (sp 0) defs
        SDef -> \case
          DefF fn args body -> showParen (p > 10) $
            showString "DefF " . showsPrec 11 fn . showString " " . showsPrec 11 args . showString " " . sp 11 body
        SExp -> \case
          EBindF e1 pat e2 -> showParen (p > 10) $
            showString "EBindF " . sp 11 e1 . showString " " . showsPrec 11 pat . showString " " . sp 11 e2
          ECaseF x alts -> showParen (p > 10) $
            showString "ECaseF " . showsPrec 11 x . showString " " . showListWith (sp 0) alts
          ESimpleF e -> showParen (p > 10) $
            showString "ESimpleF " . sp 11 e
        SSimpleExp -> \case
          SAppF fn args -> showParen (p > 10) $
            showString "SAppF " . showsPrec 11 fn . showString " " . showsPrec 11 args
          SReturnF x -> showParen (p > 10) $
            showString "SReturnF " . showsPrec 11 x
          SStoreF x -> showParen (p > 10) $
            showString "SStoreF " . showsPrec 11 x
          SFetchF x -> showParen (p > 10) $
            showString "SFetchF " . showsPrec 11 x
          SUpdateF x y -> showParen (p > 10) $
            showString "SUpdateF " . showsPrec 11 x . showString " " . showsPrec 11 y
        SAlt -> \case
          AltF cpat e -> showParen (p > 10) $
            showString "AltF " . showsPrec 11 cpat . showString " " . sp 11 e

-- ssa :: IFix AST 'Exp -> IFix AST 'Exp
-- ssa = 

funnamesAlg :: forall ix. SingI ix => AST (Const (Set Name)) ix -> Const (Set Name) ix
funnamesAlg =
  case sing @ix of
    SProgram -> \case
      ProgramF _ defs -> Const $ Set.unions $ map getConst defs
    _ -> const $ Const mempty

-- type SSAM = State (Set Name, Int)

-- ssa :: IFix AST 'Exp -> IFix AST 'Exp
-- ssa e = flip evalState (mempty, 1) $
       -- getConst (cata folder e)
    -- >> anaM unfolder (Const (mempty, e))
    -- where
        -- folder :: AST (Const (SSAM ())) ~~> Const (SSAM ())
        -- folder = undefined
        -- unfolder :: forall ix. SingI ix
                 -- => Const (Map Name Int, IFix AST 'Exp) ix
                 -- -> SSAM (AST (Const (Map Name Int, IFix AST 'Exp)) ix)
        -- unfolder = undefined

