{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Model where

-- something like 10:00 for now
data Station = Station String deriving (Show, Eq)
data Stop = Stop String deriving (Show, Eq)
data Airport = Airport String deriving (Show, Eq)

type Stand = String
type Platform = Int
type Time = String
type Terminal = String
type Gate = String

data RailPoint = RailPoint Station Platform deriving (Show, Eq)
data BusPoint  = BusPoint  Stop Stand deriving (Show, Eq)
data AirPoint  = AirPoint  Airport Terminal Gate deriving (Show, Eq)
-- data UndergroundPoint = UndergroundPoint Station Line

data Leaf = RailLeaf Station
          | BusLeaf  Stop
          | AirLeaf  Airport
          deriving (Show, Eq)

data Line = RailLine Station  Station
          | BusLine  Stop     Stop
          | AirLine  Airport  Airport
          deriving (Show, Eq)

data Network = Terminal Leaf 
                -- last station (i.e. rail Paddington or air | rail Heathrow)
             | Connection Network Network 
                -- line between two points (physical line, i.e. one rail line; 
                -- we have 4 between Didcot and Paddington, for example.)
             deriving (Show)

data PointTime a = PointAt      a Time
                 | PointAfter   a Time
                 | PointBefore  a Time
                 | PointBetween a Time Time
                 deriving (Show, Eq)

data Leg = RailLeg (PointTime RailPoint) (PointTime RailPoint)
         | BusLeg  (PointTime BusPoint)  (PointTime BusPoint)
         | AirLeg  (PointTime AirPoint)  (PointTime AirPoint)
         deriving (Show, Eq)

data Delay = DepartureDelay Leg Time
           | ArrivalDealy   Leg Time
           | OnRouteDelay   Leg Time
           deriving (Show, Eq)

data Journey = Journey [Leg] deriving (Show)

class Connected a where
    (|<->|) :: a -> a -> Network
    a |<->| b = Connection (a |->| b) (b |->| a)
    (|<=>|) :: a -> a -> Network
    (|<=>|) = (|<->|)

    (|->|)  :: a -> a -> Network

instance Connected Station where
    a |->| b  = Connection (Terminal (RailLeaf a)) (Terminal (RailLeaf b))

instance Connected Stop where
    a |->| b = Connection (Terminal (BusLeaf a)) (Terminal (BusLeaf b))
    
fgw :: Network
fgw =
    pad |<=>| rdg -- |<=>| did
    where
        pad = Station "PAD"
        oxf = Station "OXF"
        did = Station "DID"
        rdg = Station "RDG"