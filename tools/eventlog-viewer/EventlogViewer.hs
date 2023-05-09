{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import System.Directory

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input.Events
import Graphics.Vty (defAttr)
import Booster.Trace.TH
import Booster.Pattern.Base (Pattern)
import Booster.Pattern.Rewrite (RewriteResult, RewriteFailed)

type CustomUserEvents = '[RewriteResult Pattern, RewriteFailed "Rewrite"]

$(mkSumPatterns @CustomUserEvents)

type CustomUserEventData =
    (Events.Timestamp, Sum CustomUserEvents)


parseEventLog :: Events.Event -> Maybe CustomUserEventData
parseEventLog Events.Event{evTime, evSpec, evCap} =
    case evSpec of
      Events.UserBinaryMessage msg -> case runGet (decodeCustomUserEvent @CustomUserEvents) $ BL.fromStrict msg of
          Unmatched -> Nothing
          userEvent -> Just (evTime, userEvent)


readLog :: FilePath -> IO [CustomUserEventData]
readLog file =
    Events.readEventLogFromFile file
        >>= either error (pure . mapMaybe parseEventLog . sortOn Events.evTime . Events.events . Events.dat)


main :: IO ()
main = getArgs >>= \case
  [] -> error "worng args"
  filename:_ -> do 
    eventLogData <- readLog filename 
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

data TuiState =
  TuiState
  deriving (Show, Eq)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure ()
    , appAttrMap = const $ attrMap defAttr []
    }

buildInitialState :: IO TuiState
buildInitialState = pure TuiState

drawTui :: TuiState -> [Widget ResourceName]
drawTui _ts = [ str "aaa"]

handleTuiEvent :: BrickEvent n e -> EventM n TuiState ()
handleTuiEvent e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt
        _ -> continueWithoutRedraw
    _ -> return ()