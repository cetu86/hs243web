{-# LANGUAGE OverloadedStrings,RecordWildCards,NamedFieldPuns,ParallelListComp #-}
import Haste
import Haste.DOM
import Haste.Foreign
import Haste.Concurrent
import Haste.LocalStorage
import Control.Monad
import Control.Applicative
import Haste.App (MonadIO)
import Data.Tuple (swap)
import Hs243.Logic

faddChild e = (=<<) (\c -> addChild c e)

tag :: MonadIO m => String -> [(PropID,String)] -> [m Elem] -> m Elem
tag name properties children = do
    e <- newElem name 
    forM_ properties $ \(propid,val) -> case propid of 
            "class" -> setClass e val True
            _       -> setProp e propid val
    forM_ children (faddChild e)
    return e

te :: MonadIO m => String -> m Elem
te = newTextElem

randint :: MVar Seed -> Int -> CIO Int
randint a n = modifyMVarIO a $ return . swap . randomR (0,n) 

randr :: MVar Seed -> CIO Double
randr a = modifyMVarIO a $ return . swap . randomR (0,1)


main = do
    s <- newSeed
    a <- newMVar s 
    concurrent $ actualmain $ (Config (randr a) (randint a) 
                Params { dimension = 5 , base = 3, winat = 5, distribution = [0.9] } :: Config CIO)
    

actualmain :: Config CIO -> CIO ()
actualmain config@(Config _ _ params@Params {..} ) = do
    brett <- neuesElement config (leeresBrett params) >>= neuesElement config
    b <- newMVar brett


    withElem "spielfeld" $ \e-> mapM_ (const $ faddChild e (tag "tr" [] 
                                    (map (const $ tag "td" [] []) (replicate dimension 0) )
                                )) (replicate dimension 0)
    zeigeBrett brett

    let cb = concurrent . verarbeiteEingabe b . mapEingabe

    liftIO $ setCallback documentBody OnKeyDown cb

    forM_ [("clinks",37),("coben",38),("crechts",39),("cunten",40)] $ \(id,key) ->
        withElem id $ \e -> liftIO $ setCallback e OnMouseDown $ \_ _ -> cb key

    return ()

    where 
          mapEingabe :: Int -> Maybe Ar
          mapEingabe 38 = Just (Verti,Zurück)
          mapEingabe 40 = Just (Verti,Vor)
          mapEingabe 39 = Just (Hori,Vor)
          mapEingabe 37 = Just (Hori,Zurück)
          mapEingabe _ = Nothing
          verarbeiteEingabe :: MVar Brett -> Maybe Ar -> CIO ()
          verarbeiteEingabe b (Just ar) = do 
                                    brett <- takeMVar b
                                    brett' <- schritt config ar brett
                                    putMVar b brett'
                                    zeigeBrett brett'
                                    behandleRandfälle brett' 
          verarbeiteEingabe b Nothing = return ()

          behandleRandfälle brett | isOver params brett = alert "GAME OVER!"
                                  |  isWon params brett = alert "you win :-)"
                                  |    otherwise = return ()
          zeigeBrett :: [[Int]] -> CIO ()
          zeigeBrett brett = withElem "spielfeld" $ getChildren >=> 
                                sequence_ . zipWith zeigeReihe brett
            where zeigeReihe werte = getChildren >=> 
                    sequence_ . zipWith zeigeWert werte
                        where zeigeWert wert e  = do
                                clearChildren e  
                                setStyle e "background-color" (farbe wert)
                                when (wert /= 0) $ (faddChild e .(te .show)) wert 

          farben = [(237,201,81),(235,104,65),(204,51,63),(106,74,60),(0,160,176)]
          farbe x = let fs = [f | (f,n) <- zip farben [0..], base^n == x ]
                    in case fs of [] -> "white"
                                  ((r,g,b):_) -> "rgb("++ show r ++ "," 
                                                       ++ show g ++ ","
                                                       ++ show b ++ ")"
                        
    
