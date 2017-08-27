module DNAnts.State.Population where

import DNAnts.State.Ant (AntTeam)

newtype Population =
  Population [AntTeam]