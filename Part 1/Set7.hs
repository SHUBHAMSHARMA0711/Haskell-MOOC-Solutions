-- Exercise set 7

module Set7 where

import Mooc.Todo
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid
import Data.Semigroup

------------------------------------------------------------------------------
-- Ex 1: you'll find below the types Time, Distance and Velocity,
-- which represent time, distance and velocity in seconds, meters and
-- meters per second.
--
-- Implement the functions below.

data Distance = Distance Double
  deriving (Show,Eq)

data Time = Time Double
  deriving (Show,Eq)

data Velocity = Velocity Double
  deriving (Show,Eq)

-- velocity computes a velocity given a distance and a time
velocity :: Distance -> Time -> Velocity
velocity (Distance distance) (Time time) = Velocity (distance / time)

-- travel computes a distance given a velocity and a time
travel :: Velocity -> Time -> Distance
travel (Velocity velocity) (Time time)  = Distance (velocity * time)

------------------------------------------------------------------------------
-- Ex 2: let's implement a simple Set datatype. A Set is a list of
-- unique elements. The set is always kept ordered.
--
-- Implement the functions below. You might need to add class
-- constraints to the functions' types.
--
-- Examples:
--   member 'a' (Set ['a','b','c'])  ==>  True
--   add 2 (add 3 (add 1 emptySet))  ==>  Set [1,2,3]
--   add 1 (add 1 emptySet)  ==>  Set [1]

data Set x = Set [x]
  deriving (Show, Eq)

-- emptySet is a set with no elements
emptySet :: Set x
emptySet = Set []

-- member tests if an element is in a set
member :: Eq x => x -> Set x -> Bool
member element (Set orderedSet) = elem (element) (orderedSet)

-- add a member to a set
add :: Ord x => x -> Set x -> Set x
add element (Set orderedSet) = if elem (element) (orderedSet) then Set (orderedSet) else Set (sort (element : orderedSet))

------------------------------------------------------------------------------
-- Ex 3: a state machine for baking a cake. The type Event represents
-- things that can happen while baking a cake. The type State is meant
-- to represent the states a cake can be in.
--
-- Your job is to
--
--  * add new states to the State type
--  * and implement the step function
--
-- so that they have the following behaviour:
--
--  * Baking starts in the Start state
--  * A successful cake (reperesented by the Finished value) is baked
--    by first adding eggs, then adding flour and sugar (flour and
--    sugar can be added in which ever order), then mixing, and
--    finally baking.
--  * If the order of Events differs from this, the result is an Error cake.
--    No Events can save an Error cake.
--  * Once a cake is Finished, it stays Finished even if additional Events happen.
--
-- The function bake just calls step repeatedly. It's used for the
-- examples below. Don't modify it.
--
-- Examples:
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake]  ==>  Finished
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake,AddSugar,Mix]  ==> Finished
--   bake [AddFlour]  ==>  Error
--   bake [AddEggs,AddFlour,Mix]  ==>  Error

data Event = AddEggs | AddFlour | AddSugar | Mix | Bake
  deriving (Eq, Show)

data State = Start | Finished | OnlyEggsAdded | EggsAndFlourAdded | EggsAndSugarAdded | EverythingAdded | MixtureReadyToBake | Error
  deriving (Eq, Show)

step :: State -> Event -> State
step (Start) (AddEggs) = OnlyEggsAdded
step (MixtureReadyToBake) (Bake) = Finished
step (EverythingAdded) (Mix) = MixtureReadyToBake
step (OnlyEggsAdded) (AddFlour) = EggsAndFlourAdded
step (OnlyEggsAdded) (AddSugar) = EggsAndSugarAdded
step (EggsAndFlourAdded) (AddSugar) = EverythingAdded
step (EggsAndSugarAdded) (AddFlour) = EverythingAdded
step (Finished) (_) = Finished
step (_) (_) = Error

-- do not edit this
bake :: [Event] -> State
bake events = go Start events
  where go state [] = state
        go state (e:es) = go (step state e) es

------------------------------------------------------------------------------
-- Ex 4: remember how the average function from Set4 couldn't really
-- work on empty lists? Now we can reimplement average for NonEmpty
-- lists and avoid the edge case.
--
-- PS. The Data.List.NonEmpty type has been imported for you
--
-- Examples:
--   average (1.0 :| [])  ==>  1.0
--   average (1.0 :| [2.0,3.0])  ==>  2.0

average :: Fractional x => NonEmpty x -> x
average (firstElement :| restOfElements) = (firstElement + (sum (restOfElements))) / (fromIntegral ((length (restOfElements)) + 1))

------------------------------------------------------------------------------
-- Ex 5: reverse a NonEmpty list.
--
-- PS. The Data.List.NonEmpty type has been imported for you

reverseNonEmpty :: NonEmpty x -> NonEmpty x
reverseNonEmpty (firstElement :| restOfElements) = if (length (restOfElements)) == 0 then (firstElement :| []) else (last (restOfElements) :| (reverse (init (restOfElements))) ++ [firstElement])

------------------------------------------------------------------------------
-- Ex 6: implement Semigroup instances for the Distance, Time and
-- Velocity types from exercise 1. The instances should perform
-- addition.
--
-- When you've defined the instances you can do things like this:
--
-- velocity (Distance 50 <> Distance 10) (Time 1 <> Time 2)
--    ==> Velocity 20

instance Semigroup Distance where
  (Distance distance1) <> (Distance distance2) = Distance (distance1 + distance2)

instance Semigroup Time where
  (Time time1) <> (Time time2) = Time (time1 + time2)

instance Semigroup Velocity where
  (Velocity velocity1) <> (Velocity velocity2) = Velocity (velocity1 + velocity2)

------------------------------------------------------------------------------
-- Ex 7: implement a Monoid instance for the Set type from exercise 2.
-- The (<>) operation should be the union of sets.
--
-- What's the right definition for mempty?
--
-- What are the class constraints for the instances?

instance Ord x => Semigroup (Set x) where
  (Set (orderedSet1)) <> (Set (orderedSet2)) = Set (sort (nub ((orderedSet1) ++ (orderedSet2))))

instance Ord x => Monoid (Set x) where
  mempty = Set ([])

------------------------------------------------------------------------------
-- Ex 8: below you'll find two different ways of representing
-- calculator operations. The type Operation1 is a closed abstraction,
-- while the class Operation2 is an open abstraction.
--
-- Your task is to add:
--  * a multiplication case to Operation1 and Operation2
--    (named Multiply1 and Multiply2, respectively)
--  * functions show1 and show2 that render values of
--    Operation1 and Operation2 to strings
--
-- Examples:
--   compute1 (Multiply1 2 3) ==> 6
--   compute2 (Multiply2 2 3) ==> 6
--   show1 (Add1 2 3) ==> "2+3"
--   show1 (Multiply1 4 5) ==> "4*5"
--   show2 (Subtract2 2 3) ==> "2-3"
--   show2 (Multiply2 4 5) ==> "4*5"

data Operation1 = Add1 Int Int | Subtract1 Int Int | Multiply1 Int Int
  deriving Show

data Add2 = Add2 Int Int
  deriving Show

data Subtract2 = Subtract2 Int Int
  deriving Show

data Multiply2 = Multiply2 Int Int
  deriving Show

class Operation2 operation where
  show2 :: operation -> String
  compute2 :: operation -> Int

compute1 :: Operation1 -> Int
compute1 (Add1 (element1) (element2)) = element1 + element2
compute1 (Subtract1 (element1) (element2)) = element1 - element2
compute1 (Multiply1 (element1) (element2)) = element1 * element2

show1 :: Operation1 -> String
show1 (Add1 (element1) (element2)) = show (element1) ++ ("+") ++ show (element2)
show1 (Subtract1 (element1) (element2)) = show (element1) ++ ("-") ++ show (element2)
show1 (Multiply1 (element1) (element2)) = show (element1) ++ ("*") ++ show (element2)

instance Operation2 Add2 where
  show2 (Add2 (element1) (element2)) = show (element1) ++ ("+") ++ show (element2)
  compute2 (Add2 (element1) (element2)) = element1 + element2

instance Operation2 Subtract2 where
  show2 (Subtract2 (element1) (element2)) = show (element1) ++ ("-") ++ show (element2)
  compute2 (Subtract2 (element1) (element2)) = element1 - element2

instance Operation2 Multiply2 where
  show2 (Multiply2 (element1) (element2)) = show (element1) ++ ("*") ++ show (element2)
  compute2 (Multiply2 (element1) (element2)) = element1 * element2

------------------------------------------------------------------------------
-- Ex 9: validating passwords. Below you'll find a type
-- PasswordRequirement describing possible requirements for passwords.
--
-- Implement the function passwordAllowed that checks whether a
-- password is allowed.
--
-- Examples:
--   passwordAllowed "short" (MinimumLength 8) ==> False
--   passwordAllowed "veryLongPassword" (MinimumLength 8) ==> True
--   passwordAllowed "password" (ContainsSome "0123456789") ==> False
--   passwordAllowed "p4ssword" (ContainsSome "0123456789") ==> True
--   passwordAllowed "password" (DoesNotContain "0123456789") ==> True
--   passwordAllowed "p4ssword" (DoesNotContain "0123456789") ==> False
--   passwordAllowed "p4ssword" (And (ContainsSome "1234") (MinimumLength 5)) ==> True
--   passwordAllowed "p4ss" (And (ContainsSome "1234") (MinimumLength 5)) ==> False
--   passwordAllowed "p4ss" (Or (ContainsSome "1234") (MinimumLength 5)) ==> True

data PasswordRequirement =
  MinimumLength Int
  | ContainsSome String    -- contains at least one of given characters
  | DoesNotContain String  -- does not contain any of the given characters
  | And PasswordRequirement PasswordRequirement -- and'ing two requirements
  | Or PasswordRequirement PasswordRequirement  -- or'ing
  deriving Show

passwordAllowed :: String -> PasswordRequirement -> Bool
passwordAllowed (password) (MinimumLength (passwordLength)) = length (password) >= passwordLength
passwordAllowed (password) (ContainsSome (characters)) = any (\character -> elem (character) (password)) (characters)
passwordAllowed (password) (DoesNotContain (characters)) = not (any (\character -> elem (character) (password)) (characters))
passwordAllowed (password) (Or  (requirement1) (requirement2)) = (passwordAllowed (password) (requirement1)) || (passwordAllowed (password) (requirement2))
passwordAllowed (password) (And (requirement1) (requirement2)) = (passwordAllowed (password) (requirement1)) && (passwordAllowed (password) (requirement2))

------------------------------------------------------------------------------
-- Ex 10: a DSL for simple arithmetic expressions with addition and
-- multiplication. Define the type Arithmetic so that it can express
-- expressions like this. Define the functions literal and operation
-- for creating Arithmetic values.
--
-- Define two interpreters for Arithmetic: evaluate should compute the
-- expression, and render should show the expression as a string.
--
-- Examples:
--   evaluate (literal 3) ==> 3
--   render   (literal 3) ==> "3"
--   evaluate (operation "+" (literal 3) (literal 4)) ==> 7
--   render   (operation "+" (literal 3) (literal 4)) ==> "(3+4)"
--   evaluate (operation "*" (literal 3) (operation "+" (literal 1) (literal 1)))
--     ==> 6
--   render   (operation "*" (literal 3) (operation "+" (literal 1) (literal 1)))
--     ==> "(3*(1+1))"
--

data Arithmetic = Literal Integer | Operation String Arithmetic Arithmetic
  deriving Show

literal :: Integer -> Arithmetic
literal = (Literal)

operation :: String -> Arithmetic -> Arithmetic -> Arithmetic
operation ("+") (arithmetic1) (arithmetic2) = Operation ("+") (arithmetic1) (arithmetic2)
operation ("*") (arithmetic1) (arithmetic2) = Operation ("*") (arithmetic1) (arithmetic2)

evaluate :: Arithmetic -> Integer
evaluate (Literal (number)) = number
evaluate (Operation ("+") (arithmetic1) (arithmetic2)) = (evaluate (arithmetic1)) + (evaluate (arithmetic2))
evaluate (Operation ("*") (arithmetic1) (arithmetic2)) = (evaluate (arithmetic1)) * (evaluate (arithmetic2))

render :: Arithmetic -> String
render (Literal (number)) = show (number)
render (Operation ("+") (arithmetic1) (arithmetic2)) = ("(") ++ (render (arithmetic1)) ++ ("+") ++ (render (arithmetic2)) ++ (")")
render (Operation ("*") (arithmetic1) (arithmetic2)) = ("(") ++ (render (arithmetic1)) ++ ("*") ++ (render (arithmetic2)) ++ (")")
