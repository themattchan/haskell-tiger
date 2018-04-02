module Language.Tiger.Loc where

-- This is AlexPosn but in my own type because i can't figure out how to import it
data SrcPosn = SrcPosn !Int  -- absolute character offset
                       !Int  -- line number
                       !Int  -- column number

data Loc a = Loc { locPosn :: SrcPosn, locData :: a }
