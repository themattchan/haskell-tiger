{
module Language.Tiger.Parser (parseProgram) where

import Control.Monad
import Data.Semigroup
import Data.String
import qualified Data.ByteString.Lazy as BS

import Language.Tiger.Lexer as Lexer
import qualified Language.Tiger.Token as Tok
import Language.Tiger.Loc
import Language.Tiger.Types
}

%name program
%monad { Either String } { (>>=) } { return }
%error { parseError }
%tokentype { Loc Tok.Token }


%token
  INT         { Loc _ (Tok.IntLit _) }
  ID          { Loc _ (Tok.Ident _ ) }
  STRING      { Loc _ (Tok.StringLit _ ) }

  'var'       { Loc $$ Tok.Var       }
  'while'     { Loc $$ Tok.While     }
  'for'       { Loc $$ Tok.For       }
  'to'        { Loc $$ Tok.To        }
  'break'     { Loc $$ Tok.Break     }
  'let'       { Loc $$ Tok.Let       }
  'in'        { Loc $$ Tok.In        }
  'end'       { Loc $$ Tok.End       }
  'function'  { Loc $$ Tok.Function  }
  'type'      { Loc $$ Tok.Type      }
  'array'     { Loc $$ Tok.Array     }
  'if'        { Loc $$ Tok.If        }
  'then'      { Loc $$ Tok.Then      }
  'else'      { Loc $$ Tok.Else      }
  'do'        { Loc $$ Tok.Do        }
  'of'        { Loc $$ Tok.Of        }
  'nil'       { Loc $$ Tok.Nil       }
  ','         { Loc $$ Tok.Comma     }
  ':'         { Loc $$ Tok.Colon     }
  ';'         { Loc $$ Tok.Semi      }
  '('         { Loc $$ Tok.LPar      }
  ')'         { Loc $$ Tok.RPar      }
  '['         { Loc $$ Tok.LBrack    }
  ']'         { Loc $$ Tok.RBrack    }
  '{'         { Loc $$ Tok.LBrace    }
  '}'         { Loc $$ Tok.RBrace    }
  '.'         { Loc $$ Tok.Dot       }
  '+'         { Loc $$ Tok.Plus      }
  '-'         { Loc $$ Tok.Minus     }
  '*'         { Loc $$ Tok.Times     }
  '/'         { Loc $$ Tok.Divide    }
  '='         { Loc $$ Tok.Eq        }
  '<>'        { Loc $$ Tok.Neq       }
  '>'         { Loc $$ Tok.Gt        }
  '<'         { Loc $$ Tok.Lt        }
  '>='        { Loc $$ Tok.Ge        }
  '<='        { Loc $$ Tok.Le        }
  '&'         { Loc $$ Tok.And       }
  '|'         { Loc $$ Tok.Or        }
  ':='        { Loc $$ Tok.Assign    }

%right    'of'
%nonassoc 'do'
%nonassoc 'else'
%nonassoc ':='
%left     '&' '|'
%nonassoc '=' '<>' '<' '<=' '>' '>='
%left     '+' '-'
%left     '*' '/'
%left     UMINUS

%%

program :: { Exp L }
  : exp { $1 }

exp :: { Exp L }
   : lvalue      { Var $1 (sp $1) }
   | record      { $1 }
   | sequence    { $1 }
   | app         { $1 }
   | cmpexp      { $1 }
   | mathexp     { $1 }
   | boolexp     { $1 }
   | array       { $1 }
   | assign      { $1 }
   | control     { $1 }
   | 'nil'       { Nil (sp $1) }
   | INT         { let Loc l (Tok.IntLit i) = $1 in Int i (sp l) }
   | STRING      { let Loc l (Tok.StringLit s) = $1 in String s (sp l) }

decs :: { [Decl L] }
  : {- nil -}  { [] }
  | dec decs   { $1 : $2 }

dec :: { Decl L }
  : vardec  { $1 }
  | fundec  { FunctionDecl [$1] (sp $1) }
  | tydec   { TypeDecl     [$1] ((\(_,_,x) -> x) $1) }

vardec :: { Decl L }
  : 'var' ID ':=' exp
     { VarDecl (sym $2) True Nothing      $4 (spr $1 $4) }
  | 'var' ID ':' ID ':=' exp
     { VarDecl (sym $2) True (Just (sym $4, sp $4)) $6 (spr $1 $6) }

fundec :: { Function L }
  : 'function' ID '(' tyfields ')' exp
     { Function (sym $2) $4 Nothing      $6 (spr $1 $6) }
  | 'function' ID '(' tyfields ')' ':' ID exp
     { Function (sym $2) $4 (Just (sym $7, sp $7)) $8 (spr $1 $8) }

tyfields :: { [Field L] }
  : {- nil -} { [] }
  | tyfield1 tyfieldss { $1 : $2 }

tyfield1 :: { Field L }
  : ID ':' ID { Field (sym $1) (sym $3) (spr $1 $3) }

tyfieldss :: { [Field L] }
  : {- nil -} { [] }
  | ',' tyfield1 tyfieldss { $2 : $3 }

tydec :: { (Symbol, Ty L, L) }
  : 'type' ID '=' ty { (sym $2, $4, (spr $1 $4)) }

ty :: { Ty L }
  : ID                { NameTy (sym $1) Nothing (sp $1) }
  | '{' tyfields '}'  { RecordTy $2 123456789 (spr $1 $3) } -- FIXME generate unique number
  | 'array' 'of' ID   { ArrayTy  (sym $3) 123456789 (spr $1 $3) } -- FIXME generate unique number

lvalue :: { Var L }
  : ID      { SimpleVar (sym $1) (sp $1) }
  | lvalue1 { $1 }

lvalue1 :: { Var L }
  : ID '.' ID            { FieldVar (SimpleVar (sym $1) (sp $1)) (sym $3) (spr $1 $3) }
  | lvalue1 '.' ID       { FieldVar $1 (sym $3) (spr $1 $3)  }
  | ID '[' exp ']'       { SubscriptVar (SimpleVar (sym $1) (sp $1)) $3 (spr $1 $4) }
  | lvalue1 '[' exp ']'  { SubscriptVar $1 $3 (spr $1 $4) }

control :: { Exp L }
  : 'if' exp 'then' exp 'else' exp        { If $2 $4 (Just $6)     (spr $1 $6) }
  | 'if' exp 'then' exp                   { If $2 $4  Nothing      (spr $1 $4) }
  | 'while' exp 'do' exp                  { While $2 $4            (spr $1 $4) }
  | 'for' ID ':=' exp 'to' exp 'do' exp   { For (sym $2) $4 $6 $8 (spr $1 $8) }
  | 'break'                               { Break                  (sp $1)     }
  | 'let' decs 'in' expseq 'end'          { Let $2 (Seq $4 (foldMap sp $4)) (spr $1 $5) }

expseq :: { [Exp L] }
  : {- nil -}                   { []      } -- Q: are empty sequences allowed??
  | exp expseq1                 { $1 : $2 }

expseq1 :: { [Exp L] }
  : {- nil -}                   { []      }
  | ';' exp expseq1             { $2 : $3 }

app :: { Exp L }
  : ID '(' args ')' { Call (sym $1) $3 ((sp $1)<> (sp $4)) }

args :: { [Exp L] }
  : {- nil -}    { [] }
  | exp moreargs { $1 : $2 }

moreargs :: { [Exp L] }
  : {- nil -}          { [] }
  | ',' exp moreargs   { $2 : $3 }

cmpexp :: { Exp L }
  : exp '='   exp   { Op $1 Eq      $3 (spr $1 $3)  }
  | exp '<>'  exp   { Op $1 Neq     $3 (spr $1 $3)  }
  | exp '>'   exp   { Op $1 Lt      $3 (spr $1 $3)  }
  | exp '<'   exp   { Op $1 Gt      $3 (spr $1 $3)  }
  | exp '>='  exp   { Op $1 Le      $3 (spr $1 $3)  }
  | exp '<='  exp   { Op $1 Ge      $3 (spr $1 $3)  }

mathexp :: { Exp L }
  :     '-'  exp %prec UMINUS { Op (Int (-1) mempty) Times $2 (spr $1 $2)  }
  | exp '+'  exp              { Op $1 Plus   $3  (spr $1 $3)  }
  | exp '-'  exp              { Op $1 Minus  $3  (spr $1 $3)  }
  | exp '*'  exp              { Op $1 Times  $3  (spr $1 $3)  }
  | exp '/'  exp              { Op $1 Divide $3  (spr $1 $3)  }

boolexp :: { Exp L }
  : exp '&' exp                 { Op $1 And    $3  (spr $1 $3) }
  | exp '|' exp                 { Op $1 Or     $3  (spr $1 $3) }

assign :: { Exp L }
  : lvalue ':=' exp { Assign $1 $3 (spr $1 $3) }

sequence :: { Exp L }
  : '(' sequence1 ')' { Seq $2 (sp $1 <> sp $3) }

sequence1 :: { [Exp L] }
  : {- nil -}      { [] }
  | exp sequence2  { $1 : $2 }

sequence2 :: { [Exp L] }
  : {- nil -}         { [] }
  | ';' exp sequence2 { $2 : $3 }

record :: { Exp L }
  : ID '{' field1 fields '}' { Record ($3:$4) (sym $1) (spr $1 $5) }

field1 :: { (Symbol, Exp L, L) }
  : ID '=' exp { (sym $1, $3, (spr $1 $3)) }

fields :: { [(Symbol, Exp L, L)] }
  : {- nil -}         { [] }
  | ',' field1 fields { $2 : $3 }

array :: { Exp L }
  : ID '[' exp ']' 'of' exp  { Array (sym $1) $3 $6  (spr $1 $6) }


{
type L = SrcSpan

parseError :: [Loc Tok.Token] -> Either String a
parseError toks = Left $ "A parse error occurred\n\n " <> show toks

class HasSrcSpan a where
  sp :: a -> SrcSpan

instance HasSrcSpan SrcSpan where
  sp = id

instance HasSrcSpan SrcPosn where
  sp = posnToSpan

instance HasSrcSpan (Loc a) where
  sp = posnToSpan . locPosn

instance (Ann f) => HasSrcSpan (f SrcSpan) where
  sp = ann

spr :: (HasSrcSpan x, HasSrcSpan y) => x -> y -> SrcSpan
spr x y = sp x <> sp y

sym (Loc _ (Tok.Ident s)) = Sym s

parseProgram :: BS.ByteString -> Either String (Exp L)
parseProgram = program <=< Lexer.lex

}
