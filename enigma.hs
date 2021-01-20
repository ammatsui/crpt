module Enigma where


-- https://www.codesandciphers.org.uk/enigma/example1.htm
-- for some reason every simulation that I found online produces different output with the same input
-- I have no idea if what I have written works as intended
-- I am almost sure it doesn't
-- at least in enciphers 'G' as in the example above :)


import Data.Maybe
import Data.Char



type Letter = Char

data Rotor = Rotor { pos    :: Int,             -- starting position 
                     ring   :: Int,             -- ring = how many to skip
                     wiring :: String }         -- the permutation

instance Show Rotor where
  show (Rotor p r w) = (show w) ++ ", " ++ (show p)


data Reflector = Reflector { refl :: String } deriving Show


data Enigma = Enigma { l   :: Rotor,
                       m   :: Rotor,
                       r   :: Rotor,
                       rfl :: Reflector }

instance Show Enigma where
  show (Enigma l m r rfl) =     "Left Rotor  : " ++ (show l)   ++ "\n"
                             ++ "Middle Rotor : " ++ (show m)   ++ "\n"
                             ++ "Right Rotor   : " ++ (show r)   ++ "\n"
                             ++ "Reflector    : " ++ (show rfl) ++ "\n\n"



 
------------------------------------------------------ rotor is a substitution cypher but cooler

-- get the "key" string from a rotor
-- i.e. take 26 characters of the neverending cycle of that permutation starting from the start - 1 (since starting pos = 1)
getStr :: Rotor -> String
getStr (Rotor p r w) = take 26 $ drop (p - 1) (cycle w)


-- it is a substitution cypher with they "key" string
-- substitution cypher
-- substitution cypher
 
-- substitution cypher
alphabet = ['A'..'Z']

subst :: String -> Char -> Char
subst perm x = fromMaybe x $ lookup x $ zip alphabet perm

-- inverse substitution cypher
desubst :: String -> Char -> Char
desubst perm x = fromMaybe x $ lookup x $ zip perm alphabet

-- forward encipherment through a rotor (direct)
rotDir :: Rotor -> Letter -> Letter
rotDir rot c = subst (getStr rot) c

-- backward encipherment through a rotor (inverse)
rotInv :: Rotor -> Letter -> Letter
rotInv rot c = desubst (getStr rot) c


-- update rotor's current position
-- hint: it is almost the next one
upd :: Rotor -> Rotor
upd (Rotor p r w) = (Rotor (p + r - 1) r w)

update :: Enigma -> Enigma
update (Enigma l m r rfl) = Enigma (upd l) (upd m) (upd r) rfl


-- reflector in action
-- have i mentioned substitution cyphers?

-- substitution cypher
reflect :: Reflector -> Letter -> Letter
reflect (Reflector r) c = subst r c

----------------------------------------------------------- encipherment of a single letter
-- first through R
-- then through M
-- then through L
-- then reflector
-- backwards through L
-- backwards through M
-- backwards throuh R
-- ta-da!


-- forward encipherment through the three rotors
fwd :: Enigma -> Letter -> Letter
fwd (Enigma l m r rfl) c = rotDir l (rotDir m (rotDir r c))

-- backward encipherment through the three rotors
bkwd :: Enigma -> Letter -> Letter
bkwd (Enigma l m r rfl) c = rotInv r (rotInv m (rotInv l c))

encrCh :: Enigma -> Letter -> Letter
encrCh enigma@(Enigma l m r rfl) c = bkwd enigma s where s = reflect rfl $ fwd enigma c

------------------------------------------------------------- énigme
-- after each encipherment of a letter succ (pos)
encr :: Enigma -> String -> String
encr enigma []     = []
encr enigma (x:xs) = (encrCh enigma x) : (encr (update enigma) xs)


-- add some rotors! and reflectors!
-- (substitution cypher)

rotorI   = Rotor 1 1 "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
rotorII  = Rotor 1 1 "AJDKSIRUXBLHWTMCQGZNPYFVOE"
rotorIII = Rotor 1 1 "BDFHJLCPRTXVZNYEIWGAKMUSQO"

reflB    = Reflector "YRUHQSLDPXNGOKMIEBFZCWVJAT" 
reflC    = Reflector "FVPJIAOYEDRZXWGCTKUQSBNMHL" 


     