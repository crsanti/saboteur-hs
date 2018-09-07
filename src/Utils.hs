module Utils (
  getAvailableRoles
) where

import Player

getAvailableRoles :: Int -> Either String [Role]
getAvailableRoles 3  = return $ Saboteur : replicate 3 Miner
getAvailableRoles 4  = return $ Saboteur : replicate 4 Miner
getAvailableRoles 5  = return $ replicate 2 Saboteur ++ replicate 4 Miner
getAvailableRoles 6  = return $ replicate 2 Saboteur ++ replicate 5 Miner
getAvailableRoles 7  = return $ replicate 3 Saboteur ++ replicate 5 Miner
getAvailableRoles 8  = return $ replicate 3 Saboteur ++ replicate 6 Miner
getAvailableRoles 9  = return $ replicate 3 Saboteur ++ replicate 7 Miner
getAvailableRoles 10 = return $ replicate 4 Saboteur ++ replicate 7 Miner
getAvailableRoles _  = Left "getAvailableRoles :: number of players must be from 3 to 10"
