{
module Language.Tiger.Lexer where

data Token
  = Var
  | While
  | For
  | To
  | Break
  | Let
  | In
  | End
  | Function

  | Var
  | Type
  | Array
  | If
  | Then
  | Else
  | Do
  | Of
  | Nil

  | Comma
  | Colon
  | Semi
  | LPar
  | RPar
  | LBrack
  | RBrack
  | LBrace
  | RBrace
  | Dot
  | Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Neq
  | Gt
  | Lt
  | Ge
  | Le
  | And
  | Or
  | Assign

  | IntLit Int
  | Ident String
  | StringLit String
  deriving (Eq, Show)

data PosToken = PosToken { tokPos :: AlexPosn, tok :: Token }


mkT :: (String -> Token) -> AlexPosn -> String -> PosToken
mkT t p s = PosToken p (t s)

mkL l = mkT . const l
}

%wrapper "posn"


$digit = 0-9            -- digits
$lower = a-z
$upper = A-Z
$int = [$digit]+
$alpha = [$lower $upper]       -- alphabetic characters
$ident = [$alpha][_$alpha$digit]*


tiger :-
  $white+               ;
  "--".*                ;

  var         { mkL Var       }
  while       { mkL While     }
  for         { mkL For       }
  to          { mkL To        }
  break       { mkL Break     }
  let         { mkL Let       }
  in          { mkL In        }
  end         { mkL End       }
  function    { mkL Function  }
  var         { mkL Var       }
  type        { mkL Type      }
  array       { mkL Array     }
  if          { mkL If        }
  then        { mkL Then      }
  else        { mkL Else      }
  do          { mkL Do        }
  of          { mkL Of        }
  nil         { mkL Nil       }
  ","         { mkL Comma     }
  ":"         { mkL Colon     }
  ";"         { mkL Semi      }
  "("         { mkL LPar      }
  ")"         { mkL RPar      }
  "["         { mkL LBrack    }
  "]"         { mkL RBrack    }
  "{"         { mkL LBrace    }
  "}"         { mkL RBrace    }
  "."         { mkL Dot       }
  "+"         { mkL Plus      }
  "-"         { mkL Minus     }
  "*"         { mkL Times     }
  "/"         { mkL Divide    }
  "="         { mkL Eq        }
  "<>"        { mkL Neq       }
  ">"         { mkL Gt        }
  "<"         { mkL Lt        }
  ">="        { mkL Ge        }
  "<="        { mkL Le        }
  "&"         { mkL And       }
  "|"         { mkL Or        }
  ":="        { mkL Assign    }

  $digit+     { mkT (IntLit . read) }
  $alpha [$alpha $digit \_]* [\']*     { mkT Ident }
  [\"][^\"]*[\"] { mkT StringLit }
