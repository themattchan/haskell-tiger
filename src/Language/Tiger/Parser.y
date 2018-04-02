{
module Language.Tiger.Parser where
import Alex.Scan (AlexPosn(..))
import Language.Tiger.Lexer
import qualified Language.Tiger.Types as Ty
import Language.Tiger.Loc
}

%name happyParser
%tokentype { Loc Token }


%token
  INT         { Loc _ (IntLit _) }
  ID          { Loc _ (Ident _ ) } -- FIXME this needs to be converted into Symbol
  STRING      { Loc _ (StringLit _ ) }

  'var'       { Loc $$ Var       }
  'while'     { Loc $$ While     }
  'for'       { Loc $$ For       }
  'to'        { Loc $$ To        }
  'break'     { Loc $$ Break     }
  'let'       { Loc $$ Let       }
  'in'        { Loc $$ In        }
  'end'       { Loc $$ End       }
  'function'  { Loc $$ Function  }
  'type'      { Loc $$ Type      }
  'array'     { Loc $$ Array     }
  'if'        { Loc $$ If        }
  'then'      { Loc $$ Then      }
  'else'      { Loc $$ Else      }
  'do'        { Loc $$ Do        }
  'of'        { Loc $$ Of        }
  'nil'       { Loc $$ Nil       }
  ','         { Loc $$ Comma     }
  ':'         { Loc $$ Colon     }
  ';'         { Loc $$ Semi      }
  '('         { Loc $$ LPar      }
  ')'         { Loc $$ RPar      }
  '['         { Loc $$ LBrack    }
  ']'         { Loc $$ RBrack    }
  '{'         { Loc $$ LBrace    }
  '}'         { Loc $$ RBrace    }
  '.'         { Loc $$ Dot       }
  '+'         { Loc $$ Plus      }
  '-'         { Loc $$ Minus     }
  '*'         { Loc $$ Times     }
  '/'         { Loc $$ Divide    }
  '='         { Loc $$ Eq        }
  '<>'        { Loc $$ Neq       }
  '>'         { Loc $$ Gt        }
  '<'         { Loc $$ Lt        }
  '>='        { Loc $$ Ge        }
  '<='        { Loc $$ Le        }
  '&'         { Loc $$ And       }
  '|'         { Loc $$ Or        }
  ':='        { Loc $$ Assign    }

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
  | fundec  { Function [$1] (ann $1) }
  | tydec   { $1 }

vardec :: { Decl L }
  : 'var' ID ':=' exp
     { VarDecl $1 True None      $4 (spanPos (ann $1) (ann $4)) }
  | 'var' ID ':' ID ':=' exp
     { VarDecl $1 True (Just $4) $6 (spanPos (ann $1) (ann $6)) }

fundec :: { Function L }
  : 'function' ID '(' tyfields ')' exp
     { Function $2 $4 None      $6 (spanPos (ann $1) (ann $6)) }
  | 'function' ID '(' tyfields ')' ':' ID exp
     { Function $2 $4 (Just $7) $8 (spanPos (ann $1) (ann $8)) }

tyfields :: { [Field L] }
  : {- nil -} { [] }
  | tyfield1 tyfieldss { $1 : $2 }

tyfield1 :: { Field L }
  : ID ':' ID { Field $1 $3 (spanPos (ann $1) (ann $3)) }

tyfieldss :: { [Field L] }
  : {- nil -} { [] }
  | ',' tyfield1 tyfieldss { $2 : $3 }

tydec :: { (Symbol, Ty L, L) }
  : 'type' ID '=' ty { ($2, $4, spanPos (ann $1) (ann $4)) }

ty :: { Ty L }
  : ID                { NameTy $1 Nothing (ann $1) }
  | '{' tyfields '}'  { RecordTy $2 (spanPos (ann $1) (ann $3)) }
  | 'array' 'of' ID   { ArrayTy  $3 (spanPos (ann $1) (ann $3)) }

lvalue :: { Var L }
  : ID      { SimpleVar $1 (ann $1) }
  | lvalue1 { $1 }

lvalue1 :: { Var L }
  : ID '.' ID         { FieldVar (SimpleVar $1 (ann $1)) $3 (spanPos (ann $1) (ann $3)) }
  | lvalue1 '.' ID    { FieldVar $1 $3 (spanPos (ann $1) (ann $3))  }
  | ID '[' exp ']'    { SubscriptVar (SimpleVar $1 (ann $1)) $3 (spanPos (ann $1) (ann $4)) }
  | lvalue1 '[' exp ']'  { SubscriptVar $1 $3 (spanPos (ann $1) (ann $4)) }

control :: { Exp L }
  : 'if' exp 'then' exp 'else' exp        { If $2 $4 (Just $6)    (spanPos (ann $1) (ann $6)) }
  | 'if' exp 'then' exp                   { If $2 $4  Nothing     (spanPos (ann $1) (ann $4)) }
  | 'while' exp 'do' exp                  { While $2 $4           (spanPos (ann $1) (ann $4)) }
  | 'for' ID ':=' exp 'to' exp 'do' exp   { For $2 False $4 $6 $8 (spanPos (ann $1) (ann $8)) }
  | 'break'                               { Break                 (ann $1) }
  | 'let' decs 'in' expseq 'end'          { Let $2 (Seq $4)       (spanPos (ann $1) (ann $5)) }

expseq :: { [(Exp L, L)] }
  : {- nil -}                   { []                      }
  | exp ';' expseq              { (($1,$2) : $3)          }

app :: { Exp L }
  : ID '(' args ')' { Call $1 $3 (spanPos (ann $1) (ann $4)) }

args :: { [Exp L] }
  : {- nil -}    { [] }
  | exp moreargs { $1 : $2 }

moreargs :: { [Exp L] }
  : {- nil -}          { [] }
  | ',' exp moreargs   { $2 : $3 }

cmpexp :: { Exp L }
  : exp '='   exp   { Op $1 Eq      $3 (spanPos (ann $1) (ann $3))  }
  | exp '<>'  exp   { Op $1 Neq     $3 (spanPos (ann $1) (ann $3))  }
  | exp '>'   exp   { Op $1 Lt      $3 (spanPos (ann $1) (ann $3))  }
  | exp '<'   exp   { Op $1 Gt      $3 (spanPos (ann $1) (ann $3))  }
  | exp '>='  exp   { Op $1 Le      $3 (spanPos (ann $1) (ann $3))  }
  | exp '<='  exp   { Op $1 Ge      $3 (spanPos (ann $1) (ann $3))  }

mathexp :: { Exp L }
  :     '-'  exp %prec UMINUS { Int (- $2 )      (spanPos (ann $1) (ann $2))  }
  | exp '+'  exp              { Op $1 Plus   $3  (spanPos (ann $1) (ann $3))  }
  | exp '-'  exp              { Op $1 Minus  $3  (spanPos (ann $1) (ann $3))  }
  | exp '*'  exp              { Op $1 Times  $3  (spanPos (ann $1) (ann $3))  }
  | exp '/'  exp              { Op $1 Divide $3  (spanPos (ann $1) (ann $3))  }

boolexp :: { Exp L }
  : exp '&' exp                 { Op $1 And    $3  (spanPos (ann $1) (ann $3)) }
  | exp '|' exp                 { Op $1 Or     $3  (spanPos (ann $1) (ann $3)) }

assign :: { Exp L }
  : lvalue ':=' exp { Assign $1 $3 (spanPos (ann $1) (ann $3)) }

sequence :: { Exp L }
  : '(' sequence1 ')' { Seq $2 $1 }

sequence1 :: { [(Exp L, L)] }
  : {- nil -}      { [] }
  | exp sequence2  { $1 : $2 }

sequence2 :: { [ (Exp L, L) ] }
  : {- nil -}         { [] }
  | ';' exp sequence2 { $1 : $2 }

record :: { Exp L }
  : ID '{' field1 fields '}' { Record ($3:$4) $1 (spanPos (ann $1) (ann $5)) }

field1 :: { (Symbol, Exp L, L) }
  : ID '=' exp { ($1, $3, spanPos (ann $1) (ann $3)) }

fields :: { [(Symbol, Exp L, L)] }
  : {- nil -}         { [] }
  | ',' field1 fields { $1 : $2 }

array :: { Exp L }
  : ID '{' exp '}' 'of' exp  { Array $1 $3 $6 (spanPos (ann $1) (ann $6)) }


{
type L = SrcPosn

parseError :: [PosToken] -> a
parseError _ = error "Parse error"

spanPos :: L -> L -> L
spanPos beg end = beg

}
