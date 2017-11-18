module MorphismSpec where

import           Morphism
import           Test.Hspec

data Game = Game { numPlayers :: Int, numRobots :: Int }

data Front = SetPlayerName String
           | RegisterPlayer String
           | SetNumRobots Int
           | SetNumPlayers Int
           | CreateGame Game

data Back = List | NewGame

spec :: Spec
spec = describe "Morphism" $ do

  it "can map one word to another" $ do
    let morphism = Morphism [ Tr (State 0) (In SetPlayerName  [Var "player" (A String) ])  Eps          (State 0)
                            , Tr (State 0) (In RegisterPlayer [Var "player" (A String) ]) (Com List)    (State 1)
                            , Tr (State 1) (In SetNumRobots   [Var "num" (A Int) ])        Eps          (State 1)
                            , Tr (State 1) (In SetNumPlayers  [Var "num" (A Int) ])        Eps          (State 1)
                            , Tr (State 1) (In CreateGame     [Var "game" (A Game)])      (Com NewGame) (State 1)
                            ]

    morph morphism [ SetPlayerName "foo", SetPlayerName "bar", RegisterPlayer "bar" ]
     `shouldBe` [ List ]
