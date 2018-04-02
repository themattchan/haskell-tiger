{
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Tiger.Lexer (lex) where
import Control.Exception (SomeException, tryJust, evaluate)
import System.IO.Unsafe
import Prelude hiding (lex)

import Language.Tiger.Loc
import Language.Tiger.Token
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as BL8 (unpack)
}
%wrapper "posn-bytestring"


$digit = 0-9            -- digits
$lower = a-z
$upper = A-Z
@int = [$digit]+
$alpha = [$lower $upper]       -- alphabetic characters
@ident = $alpha [$alpha $digit \_]* [\']*
@stringlit = [\"][^\"]*[\"]

tiger :-
  $white+               ;
  "//"[^\n]*            ;
  "/*" ([[. \n] # \*] | \* [[. \n] # \/])* ("*")+ "/" ;

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

  @int        { mkT (IntLit . read . BL8.unpack) }
  @ident      { mkT Ident     }
  @stringlit  { mkT StringLit }


{

mkT :: (ByteString.ByteString -> Token) -> AlexPosn -> ByteString.ByteString -> Loc Token
mkT t (AlexPn x y z) s = Loc (SrcPosn x y z) (t s)

mkL :: Token -> AlexPosn -> ByteString.ByteString -> Loc Token
mkL l = mkT (const l)

lex :: ByteString.ByteString -> Either String [Loc Token]
lex = unsafePerformIO . tryJust (\(e :: SomeException) -> Just (show e)) . evaluate . alexScanTokens
}
