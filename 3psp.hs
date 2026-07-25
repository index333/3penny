import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import ThreeP
import Functions
import Control.Monad (forM)
import Data.IORef
main =  startGUI defaultConfig { jsPort = Just 8023 } setup
setup w = do
    a <-mkSpinboxWithTitle "rimDiameter(mm)" [300,700,1,500] w
    b <-mkSpinboxWithTitle "t(mm)" [0.5,3,0.1,1] w
    c <-mkSpinboxWithTitle "depth(mm)" [0,30,0.1,10] w
    le <- mkLabel "erd(mm)" w
    ie <- UI.input # set (attr "value") "show here, erd of rim"
    let sps = [x!!1|x <- [a,b,c]]
    d <-mkSpinboxWithTitle "flangeWidth(mm)" [30,100,1,60] w
    e <-mkSpinboxWithTitle "endWidth" [90,135,1,120] w
    f <-mkSpinboxWithTitle "end2flange(mm)" [10,50,1,40] w
    lo <- mkLabel "offset(mm)" w
    io <- UI.input # set (attr "value") "show here, offset(mm) "
    let sps1 = [x!!1|x <- [d,e,f]]
    g <-mkSpinboxWithTitle "erd(mm)" [100,700,1,500] w
    h <-mkSpinboxWithTitle "holes" [28,36,4,32] w
    i <-mkSpinboxWithTitle "pcd(mm)" [20,80,1,40] w
    j <-mkSpinboxWithTitle "flange width(mm)" [20,100,1,50] w
    k <-mkSpinboxWithTitle "offset(mm)" [0,20,1,10] w
    l <-mkSpinboxWithTitle "cross" [0,4,1,3] w
    ls <- mkLabel "spokeLength(mm)" w
    is <- UI.input # set (attr "value") "show here, 2 sets of spokes"
    let sps2 = [x!!1|x <- [g,h,i,j,k,l]]
    let update0 _ = do     
            values <- forM sps (\sb -> get UI.value sb >>= \v -> return (read v :: Double))
            t <- erd values >>= return . show
            element ie # set (attr "value") t
            element (g!!1) # set (attr "value") t
    mapM_ (\x -> on UI.valueChange x update0) sps
    let update1 _ = do
            values <- forM sps1 (\sb -> get UI.value sb >>= \v -> return (read v :: Double))
            t <- offset values >>= return . show
            element io # set (attr "value") t
            element (k!!1) # set (attr "value") t
            val <- get UI.value (d!!1) >>= \v -> return $ show (read v :: Double)
            element (j!!1) # set (attr "value") val
    mapM_ (\x -> on UI.valueChange x update1) sps1 
    let update2 _ = do
            values <- forM sps2 (\sb -> get UI.value sb >>= \v -> return (read v :: Double))
            t <- splen values  >>= return . show
            element is # set (attr "value") t
    mapM_ (\x -> on UI.valueChange x update2) sps2
    container1 <- UI.row [element x|x <- (a++b++c)++[le,ie]]
    container2 <- UI.row [element x | x <- (d++e++f)++[lo,io]]
    container3 <- UI.row [element x | x <- (g++h++i++j++k++l)++[ls,is]]
    mainLayout <- UI.column 
        [element container1,element container2,element container3 ]
    getBody w #+ [element mainLayout]
    return ()
