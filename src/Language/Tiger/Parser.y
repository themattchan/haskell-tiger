{
module Language.Tiger.Parser where
import Alex.Scan (AlexPosn(..))
import Language.Tiger.Lexer
import qualified Language.Tiger.Types as Ty
}

%name happyParser
%tokentype { PosToken }


%token
  VAR         { PosToken $$ Var       }
  WHILE       { PosToken $$ While     }
  FOR         { PosToken $$ For       }
  TO          { PosToken $$ To        }
  BREAK       { PosToken $$ Break     }
  LET         { PosToken $$ Let       }
  IN          { PosToken $$ In        }
  END         { PosToken $$ End       }
  FUNCTION    { PosToken $$ Function  }
  VAR         { PosToken $$ Var       }
  TYPE        { PosToken $$ Type      }
  ARRAY       { PosToken $$ Array     }
  IF          { PosToken $$ If        }
  THEN        { PosToken $$ Then      }
  ELSE        { PosToken $$ Else      }
  DO          { PosToken $$ Do        }
  OF          { PosToken $$ Of        }
  NIL         { PosToken $$ Nil       }
  ","         { PosToken $$ Comma     }
  ":"         { PosToken $$ Colon     }
  ";"         { PosToken $$ Semi      }
  "("         { PosToken $$ LPar      }
  ")"         { PosToken $$ RPar      }
  "["         { PosToken $$ LBrack    }
  "]"         { PosToken $$ RBrack    }
  "{"         { PosToken $$ LBrace    }
  "}"         { PosToken $$ RBrace    }
  "."         { PosToken $$ Dot       }
  "+"         { PosToken $$ Plus      }
  "-"         { PosToken $$ Minus     }
  "*"         { PosToken $$ Times     }
  "/"         { PosToken $$ Divide    }
  "="         { PosToken $$ Eq        }
  "<>"        { PosToken $$ Neq       }
  ">"         { PosToken $$ Gt        }
  "<"         { PosToken $$ Lt        }
  ">="        { PosToken $$ Ge        }
  "<="        { PosToken $$ Le        }
  "&"         { PosToken $$ And       }
  "|"         { PosToken $$ Or        }
  ":="        { PosToken $$ Assign    }

%%

program :: { Exp AlexPosn }
program
  : exp { $1 }

exp :: { Exp AlexPosn }
exp: lvalue      { $1 }
   | record      { $1 }
   | sequence    { $1 }
   | app         { $1 }
   | cmpexp      { $1 }
   | mathexp     { $1 }
   | boolexp     { $1 }
   | array       { $1 }
   | assign      { $1 }
   | control     { $1 }
   | NIL         { Nil $1 }
   | INT         { }
   | STRING      { } (A.StringExp(INT,pos))

decs :: { [Decl AlexPosn] }
decs: {- nil -}  { [] }
    | dec decs   { $1 : $2 }

dec :: { Decl AlexPosn }
dec
  : vardec  { $1 }
  | fundec  { $1 }
  | tydec   { $1 }


vardec: VAR ID ASSIGN exp (A.VarDec(ID, true, Option.None, exp, pos))
      | VAR ID COLON ID ASSIGN exp (A.VarDec(ID, true, Option.Some(ID)

fundec: FUNCTION ID LPAREN tyfields RPAREN exp ()
      | FUNCTION ID LPAREN tyfields RPAREN COLON ID exp ()


tyfields: {- nil -} ()
        | tyfield1 tyfieldss ()

tyfield1: ID COLON ID ()

tyfieldss: {- nil -} ()
         | COMMA tyfield1 tyfieldss ()

tydec: TYPE ID EQ ty ()

ty: ID ()
  | LBRACE tyfields RBRACE ()
  | ARRAY OF ID ()


lvalue: ID lvalue1 ()

lvalue1: {- nil -} ()
       | DOT ID lvalue1 ()
       | LBRACK exp RBRACK lvalue1 ()

control :: { Exp AlexPosn }
control
  : IF exp THEN exp ELSE exp          { If $2 $4 (Just $6)    $1 }
  | IF exp THEN exp                   { If $2 $4  Nothing     $1 }
  | WHILE exp DO exp                  { While $2 $4           $1 }
  | FOR ID ASSIGN exp TO exp DO exp   { For $2 False $4 $6 $8 $1 }
  | BREAK                             { Break                 $1 }
  | LET decs IN expseq END            { Let $2 (Seq $4)       $1 }

expseq :: { [(Exp AlexPosn, AlexPosn)] }
expseq
  : {- nil -}                         { []                      }
  | exp SEMICOLON expseq              { (($1,$2) : $3)          }

app
  : ID LPAREN args RPAREN ()

args
  : {- nil -}    ()
  | exp moreargs ()

moreargs
  : {- nil -}          ()
  | COMMA exp moreargs ()

cmpexp :: { Exp AlexPosn }
cmpexp
  : exp EQ  exp                  { Op $1 Eq      $3 $2  }
  | exp NEQ exp                  { Op $1 Neq     $3 $2  }
  | exp LT  exp                  { Op $1 Lt      $3 $2  }
  | exp GT  exp                  { Op $1 Gt      $3 $2  }
  | exp LE  exp                  { Op $1 Le      $3 $2  }
  | exp GE  exp                  { Op $1 Ge      $3 $2  }

mathexp :: { Exp AlexPosn }
mathexp
  :     MINUS  exp %prec UMINUS { Int (- $2 )      $1  }
  | exp PLUS   exp              { Op $1 Plus   $3  $2  }
  | exp MINUS  exp              { Op $1 Minus  $3  $2  }
  | exp TIMES  exp              { Op $1 Times  $3  $2  }
  | exp DIVIDE exp              { Op $1 Divide $3  $2  }

boolexp :: { Exp AlexPosn }
boolexp
  : exp AND exp { Op $1 And $3 $2 }
  | exp OR  exp { Op $1 Or  $3 $2 }

assign :: { Exp AlexPosn }
assign
  : lvalue ASSIGN exp { }

sequence :: { Exp AlexPosn }
sequence
  : LPAREN sequence1 RPAREN { Seq $2 $1 }

sequence1 :: { [(Exp AlexPosn, AlexPosn)] }
sequence1
  : {- nil -}      { [] }
  | exp sequence2  { }

sequence2: {- nil -}               ()
         | SEMICOLON exp sequence2 ()

record: ID LBRACE field1 fields RBRACE ()

field1: ID EQ exp ()

fields: {- nil -}           ()
      | COMMA field1 fields ()

array :: { }
array: ID LBRACK exp RBRACK OF exp  { }


{
parseError :: [PosToken] -> a
parseError _ = error "Parse error"

}
