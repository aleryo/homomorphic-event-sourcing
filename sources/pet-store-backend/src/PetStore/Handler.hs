module PetStore.Handler where

import           PetStore.Messages
import           Servant


listPets :: Handler Output
listPets = undefined

addPet :: Pet -> Handler Output
addPet = undefined

removePet :: Pet -> Handler Output
removePet = undefined

reset :: Handler NoContent
reset = undefined

login            :: User -> Handler Output
login = undefined

logout           :: User -> Handler Output
logout = undefined

addToBasket      :: User -> Pet -> Handler Output
addToBasket = undefined

removeFromBasket :: User -> Pet -> Handler Output
removeFromBasket = undefined

checkout         :: User -> Payment -> Handler Output
checkout = undefined

listBasket       :: User -> Handler Output
listBasket = undefined
