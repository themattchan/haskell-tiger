{
module Language.Tiger.Parser where
--import Alex.Scan (AlexPosn(..))
import Data.Semigroup
import Data.String
import qualified Language.Tiger.Lexer as Tok
import Language.Tiger.Types
import Language.Tiger.Loc
}

%name happyParser
%tokentype { Loc Tok.Token }


%token
  INT         { Loc _ (Tok.IntLit _) }
  ID          { Loc _ (Tok.Ident _ ) } -- FIXME this needs to be converted into Symbol
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
   : lvalue      { $1 }
   | record      { $1 }
   | sequence    { $1 }
   | app         { $1 }
   | cmpexp      { $1 }
   | mathexp     { $1 }
   | boolexp     { $1 }
   | array       { $1 }
   | assign      { $1 }
   | control     { $1 }

decs :: { [Decl L] }
  : {- nil -}  { [] }
  | dec decs   { $1 : $2 }

dec :: { Decl L }
  : vardec  { $1 }
  | fundec  { Function [$1] (sp $1) }
  | tydec   { $1 }

vardec :: { Decl L }
  : 'var' ID ':=' exp
     { VarDecl (symb $2) True None      $4 (spr $1 $4) }
  | 'var' ID ':' ID ':=' exp
     { VarDecl (symb $2) True (Just (symb $4, sp $4)) $6 (spr $1 $6) }

fundec :: { Function L }
  : 'function' ID '(' tyfields ')' exp
     { Function (symb $2) $4 None      $6 (spr $1 $6) }
  | 'function' ID '(' tyfields ')' ':' ID exp
     { Function (symb $2) $4 (Just (symb $7, sp $7)) $8 (spr $1 $8) }

tyfields :: { [Field L] }
  : {- nil -} { [] }
  | tyfield1 tyfieldss { $1 : $2 }

tyfield1 :: { Field L }
  : ID ':' ID { Field (symb $1) (symb $3) (spr $1 $3) }

tyfieldss :: { [Field L] }
  : {- nil -} { [] }
  | ',' tyfield1 tyfieldss { $2 : $3 }

tydec :: { (Symbol, Ty L, L) }
  : 'type' ID '=' ty { (symb $2, $4, (spr $1 $4)) }

ty :: { Ty L }
  : ID                { NameTy (symb $1) Nothing (sp $1) }
  | '{' tyfields '}'  { RecordTy $2 (spr $1 $3) }
  | 'array' 'of' ID   { ArrayTy  (symb $3) (spr $1 $3) }

lvalue :: { Var L }
  : ID      { SimpleVar (symb $1) (sp $1) }
  | lvalue1 { $1 }

lvalue1 :: { Var L }
  : ID '.' ID            { FieldVar (SimpleVar (symb $1) (sp $1)) (symb $3) (spr $1 $3) }
  | lvalue1 '.' ID       { FieldVar $1 (symb $3) (spr $1 $3)  }
  | ID '[' exp ']'       { SubscriptVar (SimpleVar (symb $1) (sp $1)) $3 (spr $1 $4) }
  | lvalue1 '[' exp ']'  { SubscriptVar $1 $3 (spr $1 $4) }

control :: { Exp L }
  : 'if' exp 'then' exp 'else' exp        { If $2 $4 (Just $6)     (spr $1 $6) }
  | 'if' exp 'then' exp                   { If $2 $4  Nothing      (spr $1 $4) }
  | 'while' exp 'do' exp                  { While $2 $4            (spr $1 $4) }
  | 'for' ID ':=' exp 'to' exp 'do' exp   { For (symb $2) $4 $6 $8 (spr $1 $8) }
  | 'break'                               { Break                  (sp $1) }
  | 'let' decs 'in' expseq 'end'          { Let $2 (Seq $4)        (spr $1 $5) }

expseq :: { [(Exp L, L)] }
  : {- nil -}                   { []                      }
  | exp ';' expseq              { (($1,$2) : $3)          }

app :: { Exp L }
  : ID '(' args ')' { Call (symb $1) $3 ((sp $1)<> (sp $4)) }

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
  :     '-'  exp %prec UMINUS { Int (- $2 )      (spr $1 $2)  }
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
  : ID '{' field1 fields '}' { Record ($3:$4) (symb $1) (spr $1 $5) }

field1 :: { (Symbol, Exp L, L) }
  : ID '=' exp { (symb $1, $3, (spr $1 $3)) }

fields :: { [(Symbol, Exp L, L)] }
  : {- nil -}         { [] }
  | ',' field1 fields { $2 : $3 }

array :: { Exp L }
  : ID '{' exp '}' 'of' exp  { Array (symb $1) $3 $6  (spr $1 $6) }


{
type L = SrcSpan

-- parseError :: [PosToken] -> a
-- parseError _ = error "Parse error"

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

symb (Loc _ (Tok.Ident s)) = Sym (fromString s)

}
