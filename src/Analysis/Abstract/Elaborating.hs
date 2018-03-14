module Analysis.Abstract.Elaborating
( Elaborating
) where

newtype Elaborating m term value effects a = Elaborating (m term value effects a)
