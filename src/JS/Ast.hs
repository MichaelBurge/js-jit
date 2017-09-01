module JS.Ast where

import Protolude

import Control.Comonad.Cofree
import GHC.Generics
import Data.Data

data Ref = Ref Int deriving (Eq, Show, Data, Typeable)

data BinopF a = BMult a a
              | BSub a a
              | BDiv a a
              | BPlus a a
              | BGt a a
              | BLt a a
              | BEq a a
              deriving (Eq, Show, Functor, Traversable, Foldable, Generic, Data, Typeable)

data Literal = LNum Double
             | LFunc Ref
             | LNull
             deriving (Eq, Show, Data, Typeable, Generic)

data Ty = TNum
        | TFunc
        | TObject
        | TNull
        deriving (Eq, Show, Generic, Data, Typeable)

data ExprF a = EBinop (BinopF a)
             | BAssign Ref a
             | ELit Literal
             | EObject [(Ref, a)]
             | ECall a [a]
             | EVar Ty Ref
               
             deriving (Eq, Show, Functor, Traversable, Foldable, Generic, Data, Typeable)
                      
data ExprAnno = ExprAnno {
  _eanno_ty :: Ty
  } deriving (Eq, Show)
                      
type Expr = Cofree ExprF ExprAnno

data Special = SFPrint Expr

data StatementF a = SIf Expr a a
                  | SSwitch Expr [(Literal, a)] a
                  | SDecl Ref
                  | SSpecial Special
data StAnno = StAnno {
  _stanno_lineNo :: Int
  } deriving (Eq, Show)
                  
type Statement = Cofree StatementF StAnno
