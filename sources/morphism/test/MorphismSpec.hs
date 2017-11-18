module MorphismSpec where

import           Game
import           Morphism
import           Test.Hspec

spec :: Spec
spec = describe "Morphism" $ do

  it "can map one word to another" $ do
    let morphism = Morphism [ Tr (State 0) (In SetPlayerName )   Eps          (State 0)
                            , Tr (State 0) (In RegisterPlayer)  (Com List)    (State 1)
                            , Tr (State 1) (In SetNumRobots  )   Eps          (State 1)
                            , Tr (State 1) (In SetNumPlayers )   Eps          (State 1)
                            , Tr (State 1) (In CreateGame    )   (Com NewGame) (State 1)
                            ]

    morphism `morph` [ SetPlayerName, SetPlayerName, RegisterPlayer ]
     `shouldBe` [ List ]
