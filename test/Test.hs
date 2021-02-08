module Test(test2, test1, repeatedTest) where
import System.Random
import Test.HUnit

-- | Compare verilated binary function with its origin, return Nothing
-- at success, or a description at a failure
test2 :: (Random a, Show a,
          Random b, Show b,
          Eq c, Show c) =>
         (a -> b -> c) -> (a -> b -> IO c) -> IO (Maybe String)
test2 goldenF testF = do op0 <- randomIO
                         op1 <- randomIO
                         test <- testF op0 op1
                         let golden = goldenF op0 op1
                         return (if test == golden
                                 then Nothing
                                 else Just ("Expecting " ++ show golden ++
                                            ", but found " ++ show test ++
                                            ". Operands are (" ++ show op0 ++
                                            ", " ++ show op1 ++
                                            ")."))

-- | Test unary function
test1 :: (Random a, Show a, Eq b, Show b) => (a -> b) -> (a -> IO b) -> IO (Maybe String)
test1 goldenF testF = do op <- randomIO
                         test <- testF op
                         let golden = goldenF op
                         return (if test == golden
                                 then Nothing
                                 else Just ("Expecting " ++ show golden ++
                                            ", but found " ++ show test ++
                                            ". Operand is " ++ show op ++ "."))
                                     

repeatedTest :: Int -> IO (Maybe String)  -> Test
repeatedTest n test = TestCase ((sequence $ replicate n test) >>= assertAllNothing)
  where assertAllNothing [] = return ()
        assertAllNothing (x:xs) | Just msg <- x = assertFailure msg
                                | Nothing  <- x = assertAllNothing xs

