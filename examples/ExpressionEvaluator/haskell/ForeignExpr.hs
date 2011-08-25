module ForeignExpr where

import System.CoreFoundation hiding (String)
import qualified System.CoreFoundation as CF
import Text.ParserCombinators.Parsec
import Control.Concurrent

import ExpressionParser

foreign export ccall processInput :: StringRef -> IO StringRef

-- The returned StringRef must be released by the caller of this function.
processInput :: StringRef -> IO StringRef
processInput input = do
    inputS <- getAndRetain input
    outputS <- processInputHelper inputS
    retainCFTypeRef outputS

processInputHelper :: CF.String -> IO CF.String
processInputHelper input = do
    inputStr <- getChars input
    let resultStr = case parse expr "<input>" inputStr of
                    Left err -> show err
                    Right result -> show result
    return $ fromChars resultStr
