{-# language LambdaCase #-}

module TypeCheckingFailure where

import Text.Printf       (printf)

data TypeCheckingFailure t
  = AppArgumentFailed
  | AppFunctionFailed
  | AppFunctionTypeFailed t
  | IncompatibleTypes -- should add two arguments
  | NotAType -- TODO
  | Unchecked

  | TODO -- this should be removed eventually

  deriving (Show)

displayTypeCheckingFailure :: Show t => TypeCheckingFailure t -> String
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
  TODO -> "TODO"
  Unchecked ->
    "This term was not type-checked"
