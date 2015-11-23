

module SimpleFFI

where


import Foreign.Ptr (FunPtr)
import Foreign.C (CDouble)


foreign import ccall "&exp" c_exp :: FunPtr (Double -> Double)
foreign import ccall "dynamic" mkFun :: FunPtr (Double -> Double) -> (Double -> Double)



expPercent = ((*) 10) . (mkFun c_exp)

foreign export ccall expPercent :: Double -> Double


-- main = do
--     putStrLn (show ((mkFun c_exp) 2.0))

