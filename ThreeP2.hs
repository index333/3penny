module ThreeP2 where
import Control.Monad
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
toD :: String -> IO Float
toD = (return . read) 
mkSpinBox atr w = do
    let [min,max,step,val] = map show atr
    s <- UI.input   # set (attr "type") "number"
                    # set (attr "min") min
                    # set (attr "max") max
                    # set (attr "step") step 
                    # set (attr "value") val
    return s
mkLabel s w = do 
    UI.span # set UI.text s
mkSpinboxWithTitle t vs w = do
    l <- mkLabel t w
    s <- mkSpinBox vs w
    return(l,s)  
