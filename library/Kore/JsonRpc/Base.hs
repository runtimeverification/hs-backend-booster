{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.JsonRpc.Base (
    module Kore.JsonRpc.Base,
) where

import Control.Exception (Exception)
import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import Deriving.Aeson (
    CamelToKebab,
    ConstructorTagModifier,
    CustomJSON (..),
    FieldLabelModifier,
    OmitNothingFields,
 )
import GHC.Generics (Generic)
import Kore.Syntax.Json (KoreJson, dummyKoreJson)
import Network.JSONRPC (
    FromRequest (..),
 )
import Numeric.Natural
import Prettyprinter qualified as Pretty

newtype Depth = Depth Natural
    deriving stock (Show, Eq)
    deriving newtype (FromJSON, ToJSON)

data ExecuteRequest = ExecuteRequest
    { state :: !KoreJson
    , maxDepth :: !(Maybe Depth)
    , cutPointRules :: !(Maybe [Text])
    , terminalRules :: !(Maybe [Text])
    }
    deriving stock (Generic, Show, Eq)
    deriving
        (FromJSON)
        via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToKebab]] ExecuteRequest

data ReqException = CancelRequest deriving stock (Show)

instance Exception ReqException

instance FromRequest (API 'Req) where
    parseParams "execute" = Just $ fmap (Execute <$>) parseJSON
    parseParams "cancel" = Just $ const $ return Cancel
    parseParams _ = Nothing

data ExecuteState = ExecuteState
    { term :: KoreJson
    , predicate :: Maybe KoreJson
    }
    deriving stock (Generic, Show, Eq)
    deriving
        (ToJSON)
        via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToKebab]] ExecuteState

dummyExecuteState :: ExecuteState
dummyExecuteState =
    ExecuteState
        { term = dummyKoreJson
        , predicate = Nothing
        }

data HaltReason
    = Branching
    | Stuck
    | DepthBound
    | CutPointRule
    | TerminalRule
    deriving stock (Generic, Show, Eq)
    deriving
        (ToJSON)
        via CustomJSON '[OmitNothingFields, ConstructorTagModifier '[CamelToKebab]] HaltReason

data ExecuteResult = ExecuteResult
    { reason :: HaltReason
    , depth :: Depth
    , state :: ExecuteState
    , nextStates :: Maybe [ExecuteState]
    , rule :: Maybe Text
    }
    deriving stock (Generic, Show, Eq)
    deriving
        (ToJSON)
        via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToKebab]] ExecuteResult

newtype Condition = Condition
    { predicate :: KoreJson
    }
    deriving stock (Generic, Show, Eq)
    deriving
        (ToJSON)
        via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToKebab]] Condition

data ReqOrRes = Req | Res

data APIMethods
    = ExecuteM
    | ImpliesM
    | SimplifyM

type family APIPayload (api :: APIMethods) (r :: ReqOrRes) where
    APIPayload 'ExecuteM 'Req = ExecuteRequest
    APIPayload 'ExecuteM 'Res = ExecuteResult

data API (r :: ReqOrRes) where
    Execute :: APIPayload 'ExecuteM r -> API r
    Cancel :: API 'Req

deriving stock instance Show (API 'Req)
deriving stock instance Show (API 'Res)
deriving stock instance Eq (API 'Req)
deriving stock instance Eq (API 'Res)

instance ToJSON (API 'Res) where
    toJSON = \case
        Execute payload -> toJSON payload

instance Pretty.Pretty (API 'Req) where
    pretty = \case
        Execute _ -> "execute"
        Cancel -> "cancel"

dummyExecuteResult :: API 'Res
dummyExecuteResult =
    Execute
        ExecuteResult
            { reason = Stuck
            , depth = Depth 0
            , state = dummyExecuteState
            , nextStates = Nothing
            , rule = Nothing
            }
