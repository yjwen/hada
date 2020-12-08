import System.Exit
import Plus
import qualified Wplus

main = do test <-  12 `Wplus.plus` 34
          let golden = 12 `plus` 34
          if test == golden
            then exitSuccess
            else exitFailure
