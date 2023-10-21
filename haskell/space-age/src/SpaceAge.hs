module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

relativeOrbitPeriod:: Planet -> Float
relativeOrbitPeriod Mercury = 0.2408467
relativeOrbitPeriod Venus = 0.61519726
relativeOrbitPeriod Earth = 1.0
relativeOrbitPeriod Mars = 1.8808158
relativeOrbitPeriod Jupiter = 11.862615
relativeOrbitPeriod Saturn = 29.447498
relativeOrbitPeriod Uranus = 84.016846
relativeOrbitPeriod Neptune = 164.79132

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (earthYears * earthSeconds)
  where earthSeconds = 31557600
        earthYears = relativeOrbitPeriod planet