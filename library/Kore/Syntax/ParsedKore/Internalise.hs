{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

Converts a @ParsedDefinition@ to a @KoreDefinition@, extracting all
data needed internally from the parsed entities.
-}

module Kore.Syntax.ParsedKore.Internalise (
    buildDefinition,
    DefinitionError(..),
) where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Kore.Definition.Base as Def
import Kore.Definition.Attributes.Reader
import Kore.Syntax.ParsedKore.Base
import Kore.Syntax.Json.Base (Id(..))


{- | Traverses all modules of a parsed definition, to build an
internal @KoreDefinition@.

Only very few validations are performed on the parsed data.
-}
buildDefinition :: ParsedDefinition -> Except DefinitionError KoreDefinition
buildDefinition def@ParsedDefinition{modules}
    = traverseModules modules $ emptyKoreDefinition (extract def)

-- | The state while traversing the module import graph. This is
-- internal only, but the definition is the result of the traversal.
data DefinitionState
    = State
      { moduleMap :: Map Text ParsedModule
      , definition :: KoreDefinition
      }

traverseModules ::
    [ParsedModule] ->
    KoreDefinition ->
    Except DefinitionError KoreDefinition
traverseModules modules start
    = definition <$>
      execStateT (descendFrom mainModule) State { moduleMap, definition = start }
  where
    moduleMap = Map.fromList [(moduleName m, m) | m <- modules ]
    mainModule = moduleName $ last modules  -- just by convention

    moduleName :: ParsedModule -> Text
    moduleName ParsedModule{name = Id n} = n

descendFrom :: Text -> StateT DefinitionState (Except DefinitionError) ()
descendFrom m = do
    State {moduleMap, definition} <- get
    case Map.lookup m (Def.modules definition) of
        Just _ -> pure () -- already internalised (present in the definition)
        Nothing -> do
            let mbModule = Map.lookup m moduleMap
            theModule <- maybe (lift . throwE $ NoSuchModule m) pure  mbModule

            -- traverse imports recursively before dealing with the current module

            lift $ throwE (GeneralError "continue here")

----------------------------------------
data DefinitionError
    = GeneralError Text
    | NoSuchModule Text
