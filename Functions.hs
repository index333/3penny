module Functions where
import Data.Complex
import Numeric (showFFloat)

formatOneDigit :: Double -> String
formatOneDigit x = showFFloat (Just 1) x ""
toD :: String -> IO Double
toD = (return . read) 
dist(a,b)= return $ magnitude(a-b)
hosei = 2.4 / 2
yen = pi * 2
fh pcd h k = return $ mkPolar  pcr (yen / (h / 2) * (k - 1) / 2)
    where pcr= (pcd / 2)
fr d h = return $ mkPolar a (yen / h * (-1))
    where a =( d / 2)
hypo (l, z) = dist (a, b)
    where   a = 0 :+ 0
            b = l :+ z
len erd h pcd flc k = do
    a <- fh pcd h k
    b <- fr erd h
    c <- dist (a,b)
    d <- hypo(c, flc)
    return (d - hosei)
splen [erd,h,pcd,fw,offset,cross] = do
    let f2fc = fc - offset
    splen0 <- len erd h pcd f2fc k
    let leftf2fc = fc + offset
    splen1 <- len erd h pcd leftf2fc k
    return (formatOneDigit splen0 ,formatOneDigit splen1)
        where   fc = fw / 2
                k = cross * 2
erd [d,t,dp] = return $ d - (dp - t) * 2
offset [fw,ew,e2f] = return $ abs $ c - d
    where   a = 0
            b = e2f
            c = b + fw / 2
            d = ew / 2
            e = b + fw
