module PetStore.Handler where

import           PetStore.Messages
import           Servant


listPets :: Handler [ Pet ]
listPets = undefined

addPet :: Pet -> Handler Output
addPet = undefined

removePet :: Pet -> Handler Output
removePet = undefined

reset :: Handler NoContent
reset = undefined
