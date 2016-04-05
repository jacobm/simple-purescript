module Main where

import Prelude

import Control.Monad.Aff (Aff(), later')
import Control.Monad.Eff (Eff())

import Halogen
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E

newtype State = State Int

initialState :: State
initialState = State 12

data Query a = Tick a | Reset a

ui :: forall g. Component State Query g
ui = component { render, eval }
  where

  render :: State -> ComponentHTML Query
  render (State n) =
    H.div_
      [ H.h1
          [ P.id_ "header" ]
          [ H.text "counter 12123" ]
      , H.p_
          [ H.text (show n) ]
      , H.div_ [H.text "dingo"]
      , H.button [ E.onClick (E.input_ Tick) ] [ H.text "✖" ]
      , H.button [ E.onClick (E.input_ Reset) ] [ H.text "Reset" ]
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (Tick next) = do
    modify (\(State n) -> State (n + 1))
    pure next
  eval (Reset next) = do
    modify (\(State n) -> State 0)
    pure next

-- | Run the app
main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- runUI ui initialState body
  setInterval 1000 $ driver (action Tick)

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a
