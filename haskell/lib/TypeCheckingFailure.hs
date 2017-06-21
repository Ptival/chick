{-# language LambdaCase #-}

module TypeCheckingFailure where

import Text.Printf    (printf)

import Term.Term

data TypeCheckingFailure t ν
  = AppArgumentFailed
  | AppFunctionFailed
  | AppFunctionTypeFailed (t ν)
  | IncompatibleTypes -- should add two arguments
  | NotAType -- TODO
  | SynthesizeLambda
  | UnboundVariable ν
  | Unchecked

  | TODO -- this should be removed eventually

  deriving (Show)

displayTypeCheckingFailure :: (Show ν, Show (t ν)) => TypeCheckingFailure t ν -> String
displayTypeCheckingFailure = \case
  AppArgumentFailed ->
    "The argument to this application did not type-check correctly"
  AppFunctionFailed ->
    "The function being applied did not type-check correctly"
  AppFunctionTypeFailed τFun ->
    printf
    "The type synthesized for the function being applied is not a Π-type:\n%s"
    (show τFun)
  IncompatibleTypes -> "TODO"
  NotAType -> "TODO"
  SynthesizeLambda -> "Cannot synthesize the type of an abstraction"
  TODO -> "TODO"
  UnboundVariable v -> printf "Unbound variable: %s" (show v)
  Unchecked ->
    "This term was not type-checked"

sadAppArg     = App (Left AppArgumentFailed)
sadAppFun     = App (Left AppFunctionFailed)
sadAppFunType = App (Left AppFunctionFailed)
sadPiTODO     = Pi  (Left TODO)
