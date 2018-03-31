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
  ID          { Loc _ (Ident _ ) }
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
  'var'       { Loc $$ Var       }
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

%%

program :: { Exp AlexPosn }
  : exp { $1 }

exp :: { Exp AlexPosn }
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

decs :: { [Decl AlexPosn] }
  : {- nil -}  { [] }
  | dec decs   { $1 : $2 }

dec :: { Decl AlexPosn }
  : vardec  { $1 }
  | fundec  { Function [$1] (ann $1) }
  | tydec   { $1 }

vardec :: { Decl AlexPosn }
  : 'var' ID ':=' exp
     { VarDecl $1 True None      $4 (spanPos (ann $1) (ann $4)) }
  | 'var' ID ':' ID ':=' exp
     { VarDecl $1 True (Just $4) $6 (spanPos (ann $1) (ann $6)) }

fundec :: { Function AlexPosn }
  : 'function' ID '(' tyfields ')' exp
     { Function $2 $4 None      $6 (spanPos (ann $1) (ann $6)) }
  | 'function' ID '(' tyfields ')' ':' ID exp
     { Function $2 $4 (Just $7) $8 (spanPos (ann $1) (ann $8)) }

tyfields :: { [Field AlexPosn] }
  : {- nil -} { [] }
  | tyfield1 tyfieldss { $1 : $2 }

tyfield1 :: { Field AlexPosn }
  : ID ':' ID { Field $1 $3 (spanPos (ann $1) (ann $3)) }

tyfieldss :: { [Field AlexPosn] }
  : {- nil -} { [] }
  | ',' tyfield1 tyfieldss { $2 : $3 }

tydec :: { (Symbol, Ty AlexPosn, AlexPosn) }
  : 'type' ID '=' ty { ($2, $4, spanPos (ann $1) (ann $4)) }

ty :: { Ty AlexPosn }
  : ID { }
  | '{' tyfields '}' { }
  | 'array' 'of' ID { }

lvalue :: {}
  : ID lvalue1 {}

lvalue1 :: {}
  : {- nil -} {}
  | '.' ID lvalue1 {}
  | '[' exp ']' lvalue1 {}

control :: { Exp AlexPosn }
  : 'if' exp 'then' exp 'else' exp        { If $2 $4 (Just $6)    (spanPos (ann $1) (ann $6)) }
  | 'if' exp 'then' exp                   { If $2 $4  Nothing     (spanPos (ann $1) (ann $5)) }
  | 'while' exp 'do' exp                  { While $2 $4           (spanPos (ann $1) (ann $4)) }
  | 'for' ID ':=' exp 'to' exp 'do' exp   { For $2 False $4 $6 $8 (spanPos (ann $1) (ann $8)) }
  | 'break'                               { Break                 (ann $1) }
  | 'let' decs 'in' expseq 'end'          { Let $2 (Seq $4)       (spanPos (ann $1) (ann $5)) }

expseq :: { [(Exp AlexPosn, AlexPosn)] }
  : {- nil -}                   { []                      }
  | exp ';' expseq              { (($1,$2) : $3)          }

app :: {}
  : ID '(' args ')' { }

args :: {}
  : {- nil -}    { }
  | exp moreargs { }

moreargs :: {}
  : {- nil -}          { }
  | ',' exp moreargs { }

cmpexp :: { Exp AlexPosn }
  : exp '='   exp   { Op $1 Eq      $3 (spanPos (ann $1) (ann $3))  }
  | exp '<>'  exp   { Op $1 Neq     $3 (spanPos (ann $1) (ann $3))  }
  | exp '>'   exp   { Op $1 Lt      $3 (spanPos (ann $1) (ann $3))  }
  | exp '<'   exp   { Op $1 Gt      $3 (spanPos (ann $1) (ann $3))  }
  | exp '>='  exp   { Op $1 Le      $3 (spanPos (ann $1) (ann $3))  }
  | exp '<='  exp   { Op $1 Ge      $3 (spanPos (ann $1) (ann $3))  }

mathexp :: { Exp AlexPosn }
  :     '-'  exp %prec UMINUS { Int (- $2 )      (spanPos (ann $1) (ann $2))  }
  | exp '+'  exp              { Op $1 Plus   $3  (spanPos (ann $1) (ann $3))  }
  | exp '-'  exp              { Op $1 Minus  $3  (spanPos (ann $1) (ann $3))  }
  | exp '*'  exp              { Op $1 Times  $3  (spanPos (ann $1) (ann $3))  }
  | exp '/'  exp              { Op $1 Divide $3  (spanPos (ann $1) (ann $3))  }

boolexp :: { Exp AlexPosn }
  : exp '&' exp                 { Op $1 And    $3  (spanPos (ann $1) (ann $3)) }
  | exp '|' exp                 { Op $1 Or     $3  (spanPos (ann $1) (ann $3)) }

assign :: { Exp AlexPosn }
  : lvalue ':=' exp { }

sequence :: { Exp AlexPosn }
  : '(' sequence1 ')' { Seq $2 $1 }

sequence1 :: { [(Exp AlexPosn, AlexPosn)] }
  : {- nil -}      { [] }
  | exp sequence2  { }

sequence2 :: { }
  : {- nil -}         {}
  | ';' exp sequence2 {}

record :: {}
  : ID '{' field1 fields '}' {}

field1 :: {}
  : ID '=' exp {}

fields :: {}
  : {- nil -}         {}
  | ',' field1 fields {}

array :: { }
  : ID '{' exp '}' OF exp  { }


{
type L = AlexPosn

parseError :: [PosToken] -> a
parseError _ = error "Parse error"

spanPos :: L -> L -> L
spanPos beg end = beg

}
