{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module LTL24 where

import Defs
import F24
import EventProp

data LTL24 = Atomic EventProp
           | Alt LTL24 LTL24
           | Neg LTL24
           | Future LTL24
           | Next LTL24
    deriving Show

instance Formula LTL24 where
    nf (Atomic ep) = Atomic (nf ep)
    nf (Neg (Neg phi)) = nf phi
    nf (Alt (Future phi) (Future psi)) = Future $ Alt (nf phi) (nf psi)
    nf phi = phi

instance Property LTL24 [Event] where
    sat _ [] = True
    sat (Atomic ep) es = sat ep (head es)
    sat (Alt phi psi) es = (sat phi es) || (sat psi es)
    sat (Neg phi) es = not (sat phi es)
    sat (Future phi) (e:es) = (sat phi (e:es)) || sat (Future phi) es
    sat (Next phi) (_:es) = if es == [] then False else sat phi (es)
    sat (Next _) [] = False



