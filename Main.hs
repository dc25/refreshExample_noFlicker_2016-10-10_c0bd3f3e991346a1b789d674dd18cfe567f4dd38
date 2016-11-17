{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Control.Monad.Trans (liftIO)
import Data.Map (Map, fromList)
import Data.Text (Text, pack)
import Data.Time.Clock (NominalDiffTime, getCurrentTime)

width :: Int
width =  1000

svgns :: Maybe Text
svgns = Just "http://www.w3.org/2000/svg"

circleAttrs :: Int -> Map Text Text
circleAttrs x = 
    fromList [ ( "cx", pack $ show x)
             , ( "r", "0.4" )
             ] 

boxAttrs :: Map Text Text
boxAttrs = 
    let scale = 10
    in fromList [ ("width" , pack $ show (width*scale))
                , ("height", pack $ show scale)
                , ("viewBox", pack ("-0.5 -0.5 " ++ show width ++ " 1"))
                , ("style" , "border:solid; margin:8em")
                ]

showCircle :: MonadWidget t m => Int -> Dynamic t Int -> m ()
showCircle _ dx = do
    let dAttrs = fmap circleAttrs dx
    elDynAttrNS' svgns "circle" dAttrs $ return ()
    return ()

main :: IO ()
main = mainWidget $ do
           let attrs = constDyn boxAttrs
               initial = fromList [(x,x) | x <- [0..width-1]]
           tickEvent <- tickLossy 2 =<< liftIO getCurrentTime
           dCircles <- foldDyn (flip const) initial tickEvent
           elDynAttrNS' svgns "svg" attrs $ listWithKey dCircles showCircle
           return ()
