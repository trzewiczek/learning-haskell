module Chapter_02 where
import Euterpea

-- Exercise 2.1
twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = let long t  = transpose t (note (d * 2) p)
                     short t = transpose t (note d p)
                     two     = short 2 :=: short  5 :=: short  9
                     five    = short 7 :=: short 10 :=: short 14
                     one     = long  0 :=: long   4 :=: long   7
                 in two :+: five :+: one


-- Exercise 2.2
data BluesPitchClass =
    Ro
  | MT
  | Fo
  | Fi
  | MS

type BluesPitch = (BluesPitchClass, Octave)

ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro, o)
mt o d = note d (MT, o)
fo o d = note d (Fo, o)
fi o d = note d (Fi, o)
ms o d = note d (MS, o)

fromBluesPitchClass :: BluesPitchClass -> PitchClass
fromBluesPitchClass bpc = case bpc of
  Ro -> C
  MT -> Ef
  Fo -> F
  Fi -> G
  MS -> Bf

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d (bpc, o))) = Prim (Note d (fromBluesPitchClass(bpc), o))
fromBlues (Prim (Rest d))          = Prim (Rest d)
fromBlues (m1 :+: m2)              = fromBlues(m1) :+: fromBlues(m2)
fromBlues (m1 :=: m2)              = fromBlues(m1) :=: fromBlues(m2)
fromBlues (Modify c m)             = Modify c (fromBlues m)

bluesTest :: Music Pitch
bluesTest = fromBlues(ro 3 qn :=: fo 3 qn :+: ms 3 qn :=: fo 3 qn)


-- Exercise 2.5
transM :: AbsPitch -> Music Pitch -> Music Pitch
transM ap (Prim (Note d p)) = Prim (Note d (trans ap p))
transM ap (Prim (Rest d))   = Prim (Rest d)
transM ap (m1 :+: m2)       = transM ap m1 :+: transM ap m2
transM ap (m1 :=: m2)       = transM ap m1 :=: transM ap m2
transM ap (Modify c m)      = Modify c (transM ap m)