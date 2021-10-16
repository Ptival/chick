-- {-# LANGUAGE OverloadedStrings #-}

-- module Typing.Inductive where

-- import Control.Monad.Except
-- import Control.Monad.State
-- import Inductive.Inductive
-- import PrettyPrinting.PrettyPrintable
-- import PrettyPrinting.PrettyPrintableUnannotated
-- import Term.Binder
-- import Term.Raw as Raw
-- import Term.Term
-- import Term.TypeChecked as C
-- import Text.Printf
-- import Typing.LocalContext
-- import Work

-- addVariable ::
--   MonadState (TypeCheckedLocalContext Variable) m =>
--   (α, Variable, C.Type Variable) ->
--   m ()
-- addVariable (_, v, τ) = modify $ addLocalAssum (Binder (Just v), τ)

-- addVariable' ::
--   MonadState (TypeCheckedLocalContext Variable) m =>
--   (Variable, C.Type Variable) ->
--   m ()
-- addVariable' (v, τ) = modify $ addLocalAssum (Binder (Just v), τ)

-- addBinder :: MonadState (TypeCheckedLocalContext ν) m => (Binder ν, C.Type ν) -> m ()
-- addBinder = modify . addLocalAssum

-- todo :: a
-- todo = error "TODO"

-- checkConstructor ::
--   ( MonadState (TypeCheckedLocalContext Variable) m,
--     MonadError String m,
--     MonadFix m
--   ) =>
--   Φips (Checked Variable) Variable ->
--   Φiis (Checked Variable) Variable ->
--   Inductive (Checked Variable) Variable ->
--   Constructor Raw Variable ->
--   m (Constructor (Checked Variable) Variable)
-- checkConstructor
--   ips'
--   iis'
--   ind' -- those have been checked already
--   (Constructor _ n cps cis) = do
--     -- constructors should specify all indices
--     let nbIndices = length iis'
--     when (length cis /= nbIndices) $
--       throwError $
--         printf
--           "Constructor %s should have %s indices"
--           (prettyStr n)
--           (show nbIndices)

--     -- add all parameters as local variables
--     forM_ ips' addVariable

--     -- check the arguments
--     cps' <- forM cps $ \(α, v, τ) -> do
--       ctxt <- get
--       case tc (checkF ctxt τ Type id) of
--         Left l ->
--           throwError $
--             printf
--               "In constructor %s: could not typecheck argument %s\nFail: %s\nContext: %s"
--               (prettyStr n)
--               (prettyStrU τ)
--               (show l)
--               (show ctxt)
--         Right r -> do
--           --addBinder (b, r)
--           addVariable (α, v, r)
--           return (C.Checked todo, v, r)

--     -- check the indices
--     ctxt <- get
--     cis' <- forM (zip iis' cis) $ \((α, v, τ), (_, t)) -> do
--       case tc (checkF ctxt t τ id) of
--         Left l ->
--           throwError $
--             printf
--               "In constructor %s: could not typecheck index %s at type %s\nFail: %s"
--               (prettyStr n)
--               (prettyStrU t)
--               (prettyStrU τ)
--               (show l)
--         Right r -> do
--           addVariable (α, v, r)
--           return (C.Checked todo, r)

--     return $ Constructor ind' n cps' cis'

-- checkInductive ::
--   ( MonadState (TypeCheckedLocalContext Variable) m,
--     MonadError String m,
--     MonadFix m
--   ) =>
--   Inductive Raw Variable ->
--   m (Inductive (Checked Variable) Variable)
-- checkInductive (Inductive n ips iis cs) = mfix $ \ind' -> do
--   -- checking the parameters
--   ips' <- forM ips $ \((), b, p) -> do
--     ctxt <- get
--     case tc (checkF ctxt p Type id) of
--       Left _ ->
--         throwError $
--           printf
--             "In inductive %s: could not typecheck parameter %s"
--             (prettyStr n)
--             (prettyStrU p)
--       Right r -> return (C.Checked todo, b, r)

--   -- checking the indices
--   iis' <- forM iis $ \((), b, i) -> do
--     ctxt <- get
--     case tc (checkF ctxt i Type id) of
--       Left _ ->
--         throwError $
--           printf
--             "In inductive %s: could not typecheck index type %s in context %s"
--             (prettyStr n)
--             (prettyStrU i)
--             (prettyStrU ctxt)
--       Right r -> return (C.Checked todo, b, r)

--   -- adding the inductive type to the global environment, so that constructors
--   -- may refer to it
--   addVariable' (n, inductiveType' ips' iis')

--   ctxt <- get
--   -- checking all constructors (they should not change the global context)
--   cs' <- forM cs $ \c -> do
--     (c', _) <- runStateT (checkConstructor ips' iis' ind' c) ctxt
--     return c'

--   -- now adding all the constructors to the global environment
--   --forM_ cs' $ \ (Constructor _ cn cps cis) ->
--   --  addVariable (cn, constructorTypeChecked indn ps' cps cis)

--   return (Inductive n ips' iis' cs')

-- -- addInductive ::
-- --   Inductive Raw Variable -> GlobalEnvironment (Checked Variable) Variable ->
-- --   Either String (GlobalEnvironment (Checked Variable) Variable)
-- -- addInductive i ge =
-- --   case runStateT (checkInductive i) (toLocalContext ge) of
-- --   Left  l       -> Left l
-- --   Right (i', _) -> Right $ addGlobalInd i' ge
-- --
-- -- -- add inductives from left to right
-- -- addInductives ::
-- --   [Inductive Raw Variable] -> GlobalEnvironment (Checked Variable) Variable ->
-- --   Either String (GlobalEnvironment (Checked Variable) Variable)
-- -- addInductives = flip $ foldM (flip addInductive)
