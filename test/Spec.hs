{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified App.Client as Client
import qualified Data.Map.Strict as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog
import Servant.Client (ClientEnv, ClientError (FailureResponse), ClientM, runClientM)
import Servant.Client.Core (ResponseF (..))
import System.Exit (exitFailure, exitSuccess)

import App.Client (CreateProject (..), Project (..), ProjectId (..))
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.List (sort)
import Data.Map.Strict (Map)
import Network.HTTP.Types.Status (Status (statusCode))


main :: IO ()
main = do
    ok <- tests
    if ok then exitSuccess else exitFailure


tests :: IO Bool
tests =
    checkSequential $
        Group
            "State Machine Tests"
            [ ("prop_api_tests", prop_api_tests)
            ]


prop_api_tests :: Property
prop_api_tests = withTests 100 $
    property $ do
        clientEnv <- evalIO (Client.initClientEnv 8080)
        let commands =
                [ createProjectCmd clientEnv
                , createProjectFailFullCapacityCmd clientEnv
                , createProjectFailNameExistsCmd clientEnv
                , getProjectsCmd clientEnv
                , deleteExistingProjectCmd clientEnv
                , deleteNonExistentProjectCmd clientEnv
                ]

        actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState commands

        evalEither =<< evalIO (runClientM Client.reset clientEnv)
        --evalIO $ print actions
        executeSequential initialState actions


initialState :: State v
initialState = State Map.empty


----------------------------
-- Create project (SUCCESS)
----------------------------
newtype CreateProjectInput (v :: Type -> Type)
    = CreateProjectInput String
    deriving (Eq, Show)


instance HTraversable CreateProjectInput where
    htraverse _ (CreateProjectInput name) =
        pure (CreateProjectInput name)


createProjectCmd :: (MonadGen gen, MonadTest m, MonadIO m) => ClientEnv -> Command gen m State
createProjectCmd clientEnv =
    let gen :: MonadGen gen => State Symbolic -> Maybe (gen (CreateProjectInput Symbolic))
        gen (State m)
            | Map.size m < 10 =
                let newProjectNameGen =
                        let existing = existingProjectNames (State m)
                         in Gen.filterT (`notElem` existing) $
                                Gen.string (Range.linear 1 10) Gen.alphaNum
                 in Just $ CreateProjectInput <$> newProjectNameGen
            | otherwise = Nothing

        execute :: (MonadTest m, MonadIO m) => CreateProjectInput Concrete -> m ProjectId
        execute (CreateProjectInput projName) =
            evalClientExpectSuccess clientEnv $
                Client.createProject (CreateProject projName)
     in Command
            gen
            execute
            [ Require $ \(State m) (CreateProjectInput projName) ->
                Map.size m < 10 && projName `notElem` m
            , Update $ \(State m) (CreateProjectInput projName) projId ->
                State $ Map.insert projId projName m
            , Ensure $ \(State stBefore) (State stAfter) (CreateProjectInput projName) projId -> do
                -- Project ID is not in the model BEFORE command is executed
                assert $ Map.notMember (Var $ Concrete projId) stBefore
                -- Project ID is in the model AFTER command is executed
                assert $ Map.member (Var $ Concrete projId) stAfter
                -- Project name is in the model AFTER command is executed
                assert $ projName `elem` stAfter
            ]


-------------------------------------------------
-- Create project - FAILURE due to full capacity
-------------------------------------------------
newtype CreateProjectFailFullCapacityInput (v :: Type -> Type)
    = CreateProjectFailFullCapacityInput String
    deriving (Eq, Show)


instance HTraversable CreateProjectFailFullCapacityInput where
    htraverse _ (CreateProjectFailFullCapacityInput name) =
        pure (CreateProjectFailFullCapacityInput name)


createProjectFailFullCapacityCmd :: (MonadGen gen, MonadTest m, MonadIO m) => ClientEnv -> Command gen m State
createProjectFailFullCapacityCmd clientEnv =
    let gen :: MonadGen gen => State Symbolic -> Maybe (gen (CreateProjectFailFullCapacityInput Symbolic))
        gen (State m)
            | Map.size m >= 10 =
                let newProjectNameGen =
                        let existing = existingProjectNames (State m)
                         in Gen.filterT (`notElem` existing) $
                                Gen.string (Range.linear 1 10) Gen.alphaNum
                 in Just $ CreateProjectFailFullCapacityInput <$> newProjectNameGen
            | otherwise = Nothing

        execute :: (MonadTest m, MonadIO m) => CreateProjectFailFullCapacityInput Concrete -> m ClientError
        execute (CreateProjectFailFullCapacityInput projName) =
            evalClientExpectError clientEnv $
                Client.createProject (CreateProject projName)
     in Command
            gen
            execute
            [ Require $ \(State m) (CreateProjectFailFullCapacityInput _projName) ->
                Map.size m >= 10
            , -- No need for Update callback - failure to create should not change the model
              Ensure $ \_stBefore _stAfter (CreateProjectFailFullCapacityInput _projName) clientErr ->
                assertClientError 500 "Failed to create project: storage full" clientErr
            ]


-------------------------------------------------------
-- Create project - FAILURE due to name already exists
-------------------------------------------------------
newtype CreateProjectFailNameExistsInput (v :: Type -> Type)
    = CreateProjectFailNameExistsInput String
    deriving (Eq, Show)


instance HTraversable CreateProjectFailNameExistsInput where
    htraverse _ (CreateProjectFailNameExistsInput name) =
        pure (CreateProjectFailNameExistsInput name)


createProjectFailNameExistsCmd :: (MonadGen gen, MonadTest m, MonadIO m) => ClientEnv -> Command gen m State
createProjectFailNameExistsCmd clientEnv =
    let gen :: MonadGen gen => State Symbolic -> Maybe (gen (CreateProjectFailNameExistsInput Symbolic))
        gen (State m)
            | 0 < Map.size m && Map.size m < 10 =
                let projectNameGen = Gen.element $ existingProjectNames (State m)
                 in Just $ CreateProjectFailNameExistsInput <$> projectNameGen
            | otherwise = Nothing

        execute :: (MonadTest m, MonadIO m) => CreateProjectFailNameExistsInput Concrete -> m ClientError
        execute (CreateProjectFailNameExistsInput projName) =
            evalClientExpectError clientEnv $
                Client.createProject (CreateProject projName)
     in Command
            gen
            execute
            [ Require $ \(State m) (CreateProjectFailNameExistsInput projName) ->
                projName `elem` m
            , -- No need for Update callback - failure to create should not change the model
              Ensure $ \_stBefore _stAfter (CreateProjectFailNameExistsInput _projName) clientErr ->
                assertClientError 409 "Failed to create project: name already exists" clientErr
            ]


----------------
-- Get projects
----------------
data GetProjectsInput (v :: Type -> Type)
    = GetProjectsInput
    deriving (Eq, Show)


instance HTraversable GetProjectsInput where
    htraverse _ GetProjectsInput =
        pure GetProjectsInput


getProjectsCmd :: (MonadGen gen, MonadTest m, MonadIO m) => ClientEnv -> Command gen m State
getProjectsCmd clientEnv =
    let gen :: MonadGen gen => State Symbolic -> Maybe (gen (GetProjectsInput Symbolic))
        gen _ = Just $ pure GetProjectsInput

        execute :: (MonadTest m, MonadIO m) => GetProjectsInput Concrete -> m [Project]
        execute GetProjectsInput =
            evalClientExpectSuccess clientEnv Client.getProjects
     in Command
            gen
            execute
            [ -- No need for Require callback - this command can be executed regardless of state

              -- No need for Update callback - we're just querying the state of external system without modifying it
              Ensure $ \stBefore stAfter GetProjectsInput projList -> do
                stBefore === stAfter
                sort projList === sort (toProjectList stAfter)
            ]


----------------------------
-- Delete project (SUCCESS)
----------------------------
newtype DeleteExistigProjectInput (v :: Type -> Type)
    = DeleteExistigProjectInput (Var ProjectId v)
    deriving (Eq, Show)


instance HTraversable DeleteExistigProjectInput where
    htraverse f (DeleteExistigProjectInput projId) =
        DeleteExistigProjectInput <$> htraverse f projId


deleteExistingProjectCmd :: (MonadGen gen, MonadTest m, MonadIO m) => ClientEnv -> Command gen m State
deleteExistingProjectCmd clientEnv =
    let gen :: MonadGen gen => State Symbolic -> Maybe (gen (DeleteExistigProjectInput Symbolic))
        gen (State m) = case Map.keys m of
            -- When there's no project, we can generate a command to delete a project
            [] -> Nothing
            nonEmptyProjects -> Just $ DeleteExistigProjectInput <$> Gen.element nonEmptyProjects

        execute :: (MonadTest m, MonadIO m) => DeleteExistigProjectInput Concrete -> m ()
        execute (DeleteExistigProjectInput projId) =
            evalClientExpectSuccess clientEnv $ Client.deleteProject (concrete projId)
     in Command
            gen
            execute
            [ Require $ \(State m) (DeleteExistigProjectInput projId) ->
                Map.member projId m
            , Update $ \(State m) (DeleteExistigProjectInput projId) _ ->
                State $ Map.delete projId m
            , -- No need for Update callback - we're just querying the state of external system without modifying it
              Ensure $ \(State stBefore) (State stAfter) (DeleteExistigProjectInput projId) _ -> do
                assert $ Map.member projId stBefore
                assert $ Map.notMember projId stAfter
            ]


---------------------------------------------------
-- Delete project - FAILURE due to nonexistent ID
---------------------------------------------------
newtype DeleteNonExistentProjectInput (v :: Type -> Type)
    = DeleteNonExistentProjectInput [Var ProjectId v]
    deriving (Eq, Show)


instance HTraversable DeleteNonExistentProjectInput where
    htraverse f (DeleteNonExistentProjectInput existingIds) =
        DeleteNonExistentProjectInput <$> traverse (htraverse f) existingIds


deleteNonExistentProjectCmd :: (MonadGen gen, MonadTest m, MonadIO m) => ClientEnv -> Command gen m State
deleteNonExistentProjectCmd clientEnv =
    let gen :: MonadGen gen => State Symbolic -> Maybe (gen (DeleteNonExistentProjectInput Symbolic))
        gen (State m) =
            let existingIds = Map.keys m
             in Just $ pure (DeleteNonExistentProjectInput existingIds)

        execute :: (MonadTest m, MonadIO m) => DeleteNonExistentProjectInput Concrete -> m ClientError
        execute (DeleteNonExistentProjectInput existingIds) =
            let nonExistentId = ProjectId $ case unProjectId . concrete <$> existingIds of
                    [] -> 0
                    ids -> maximum ids + 1
             in evalClientExpectError clientEnv $ Client.deleteProject nonExistentId
     in Command
            gen
            execute
            [ -- No need for Require callback - we can always try to delete nonexistent project id
              -- No need for Update callback - failure to delete nonexistent project shouldn't change app state

              Ensure $ \stBefore stAfter (DeleteNonExistentProjectInput _existingIds) clientError -> do
                stBefore === stAfter
                assertClientError 404 "Failed to delete project: ID doesn't exist" clientError
            ]


-- newtype State = State (Map ProjectId String)

newtype State v
    = State (Map (Var ProjectId v) String)
    deriving (Eq, Ord, Show)


toProjectList :: State Concrete -> [Project]
toProjectList (State m) =
    (\(projIdVar, projName) -> Project (concrete projIdVar) projName) <$> Map.toList m


existingProjectNames :: State v -> [String]
existingProjectNames (State m) = Map.elems m


evalClientExpectSuccess :: (MonadTest m, MonadIO m) => ClientEnv -> ClientM b -> m b
evalClientExpectSuccess clientEnv action = evalEither =<< evalIO (runClientM action clientEnv)


evalClientExpectError :: (MonadTest m, MonadIO m, Show a) => ClientEnv -> ClientM a -> m ClientError
evalClientExpectError clientEnv action = do
    res <- evalIO (runClientM action clientEnv)
    evalEither $ case res of
        Left err -> Right err
        Right a -> Left a


assertClientError :: MonadTest m => Int -> ByteString -> ClientError -> m ()
assertClientError expectedStatus expectedBody clientError = case clientError of
    FailureResponse _ (Response status _ _ body) ->
        (statusCode status, body) === (expectedStatus, expectedBody)
    e -> annotate ("Unexpected client error " <> show e) >> failure
