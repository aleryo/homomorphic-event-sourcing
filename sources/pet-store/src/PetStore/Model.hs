{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
module PetStore.Model where

import           Data.List         (delete)
import           Data.Maybe        (fromJust)
import           Data.Monoid       ((<>))
import           IOAutomaton
import           PetStore.Messages

-- | Concrete state of a `PetStore`
data PetStore = PetStore { storedPets :: [ Pet ]
                         , baskets    :: [ (User, [ Pet ]) ]
                         }
              deriving (Eq, Show)

-- | Abstract state of a `PetStore`
-- It is either @Open@ or invalid @Sink@
data PetStoreState = PetStoreOpen
                   | Sink
                     -- ^Special /sink/ state where all unhandled transitions endup
                   deriving (Eq, Show)

petStore :: Input
         -> PetStore
         -> (Maybe Output, PetStore)

petStore Add{pet}  store@PetStore{storedPets}
  | pet `notElem` storedPets = (Just $ PetAdded pet, store { storedPets = pet:storedPets} )
  | otherwise                = (Just $ Error PetAlreadyAdded, store)

petStore Remove{pet}  store@PetStore{storedPets}
  | pet `notElem` storedPets = (Just $ Error PetDoesNotExist, store)
  | otherwise                = (Just $ PetRemoved pet, store { storedPets = delete pet storedPets } )

-- Actually a Query not a Command
petStore ListPets          s@PetStore{storedPets}
  = (Just $ Pets storedPets, s)

petStore UserLogin { user }               store@PetStore{storedPets, baskets}
  | user `elem` fmap fst baskets = (Just $ UserLoggedIn user, store)
  | otherwise                    = (Just $ UserLoggedIn user, store { baskets = (user, []):baskets } )

petStore AddToBasket { user, pet }        store@PetStore{storedPets, baskets}
  | user `notElem` fmap fst baskets = (Just $ Error UserNotLoggedIn, store)
  | pet `notElem` storedPets        = (Just $ Error PetDoesNotExist, store)
  | otherwise                       = (Just $ AddedToBasket user pet, store { storedPets = delete pet storedPets
                                                                            , baskets = basketWithAddedPet })
  where
    addPet (u,ps) | u == user    = (u,pet:ps)
                  | otherwise    = (u,ps)
    basketWithAddedPet = fmap addPet baskets

petStore RemoveFromBasket { user, pet}    store@PetStore{storedPets, baskets}
  | user `notElem` fmap fst baskets  = (Just $ Error UserNotLoggedIn, store)
  | fmap (pet `notElem`) userBasket
    == Just True                     = (Just $ Error UserNotLoggedIn, store)
  | otherwise                        = (Just $ AddedToBasket user pet, store { storedPets = delete pet storedPets
                                                                            , baskets = basketWithRemovedPet })
  where
    userBasket = lookup user baskets

    removePet (u,ps) | u == user    = (u,pet:ps)
                     | otherwise    = (u,ps)
    basketWithRemovedPet = fmap removePet baskets

petStore CheckoutBasket { user, payment } store@PetStore{storedPets, baskets}
  | user `notElem` fmap fst baskets  = (Just $ Error UserNotLoggedIn, store)
  | otherwise                        = undefined

petStore UserLogout { user }              store@PetStore{storedPets, baskets}
  | user `notElem` fmap fst baskets  = (Just $ Error UserNotLoggedIn, store)
  | otherwise                        = (Just $ UserLoggedOut user, putBackBasketInStore )
  where
    putBackBasketInStore = undefined



instance IOAutomaton PetStore PetStoreState Input Output where
  init       = PetStore [] []
  sink       = const Sink
  state      = const PetStoreOpen
  update a _ = a
  action     = petStore

petsNames :: [ String ]
petsNames = [ "Bailey", "Bella", "Max", "Lucy", "Charlie", "Molly", "Buddy", "Daisy" ]

instance Inputs PetStore Input where
  inputs (PetStore pets _) = fmap Add listOfPets
                             <> fmap Remove pets
                             <> fmap Remove [head listOfPets ] -- might not exist
    where
      listOfPets = [ Pet name species price | name <- petsNames, species <- enumFrom Cat, price <- [ 10, 20 .. 100 ] ]
