module Test(test1, test2, test3, repeatedTest) where
import System.Random
import Test.HUnit
import Data.List

data ErrorInfo = ErrorInfo [String] String -- Operand strings and error string

addOperand :: (Show a) => a -> ErrorInfo -> ErrorInfo
addOperand a (ErrorInfo ops err) = ErrorInfo ((show a):ops) err


instance Show ErrorInfo where
  show (ErrorInfo ops err)
    | (op:[]) <- ops = err ++ " Operand is " ++ op
    | otherwise = err ++ " Operands are " ++ (intercalate ", " ops)

runTest1 :: (Random a, Show a, Eq b, Show b) => (a -> b) -> (a -> IO b) ->
            IO (Maybe ErrorInfo)
runTest1 goldenF testF = do op <- randomIO
                            test <- testF op
                            let golden = goldenF op
                            return (if test == golden
                                    then Nothing
                                    else Just (ErrorInfo
                                                [show op]
                                                ("Expecting " ++ show golden ++
                                                 ", but got " ++ show test ++ ".")))

runTest2 :: (Random a, Show a, Random b, Show b, Eq c, Show c) =>
            (a -> b -> c) -> (a -> b -> IO c) -> IO (Maybe ErrorInfo)
runTest2 goldenF testF = do a <- randomIO
                            (fmap $ fmap $ addOperand a) $ runTest1 (goldenF a) (testF a)

runTest3 :: (Random a, Show a, Random b, Show b, Random c, Show c, Eq d, Show d) =>
            (a -> b -> c -> d) -> (a -> b -> c -> IO d) -> IO (Maybe ErrorInfo)
runTest3 goldenF testF = do a <- randomIO
                            (fmap $ fmap $ addOperand a) $ runTest2 (goldenF a) (testF a)

-- | Test unary function
test1 :: (Random a, Show a, Eq b, Show b) => (a -> b) -> (a -> IO b) -> IO (Maybe String)
test1 goldenF testF = fmap (fmap show) $ runTest1 goldenF testF

test2 :: (Random a, Show a, Random b, Show b, Eq c, Show c) =>
         (a -> b -> c) -> (a -> b -> IO c) -> IO (Maybe String)
test2 goldenF testF = fmap (fmap show) $ runTest2 goldenF testF

test3 :: (Random a, Show a, Random b, Show b, Random c, Show c, Eq d, Show d) =>
         (a -> b -> c -> d) -> (a -> b -> c -> IO d) -> IO (Maybe String)
test3 goldenF testF = fmap (fmap show) $ runTest3 goldenF testF

repeatedTest :: Int -> IO (Maybe String)  -> Test
repeatedTest n test = TestCase ((sequence $ replicate n test) >>= assertAllNothing)
  where assertAllNothing [] = return ()
        assertAllNothing (x:xs) | Just msg <- x = assertFailure msg
                                | Nothing  <- x = assertAllNothing xs

