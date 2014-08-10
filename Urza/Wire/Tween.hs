{-# LANGUAGE Rank2Types #-}
module Urza.Wire.Tween where

import Prelude hiding ((.), id)
import Control.Wire
import Control.Wire.Core



linearTween :: TweenWire
linearTween = tweenWire $ \c t b -> c * t + b


easeInQuad :: TweenWire
easeInQuad = tweenWire $ \c t b -> c * t*t + b


easeOutQuad :: TweenWire
easeOutQuad = tweenWire $ \c t b -> (-c) * (t * (t - 2)) + b


easeInOutQuad :: TweenWire
easeInOutQuad = easeInOut easeInQuad easeOutQuad


easeInCubic :: TweenWire
easeInCubic = tweenWire $ \c t b -> c * t*t*t + b


easeOutCubic :: TweenWire
easeOutCubic = tweenWire $ \c t b ->
    let t' = t - 1
    in c * (t'*t'*t' + 1) + b


easeInOutCubic :: TweenWire
easeInOutCubic = easeInOut easeInCubic easeOutCubic


easeInPow :: Int -> TweenWire
easeInPow pow = tweenWire $ \c t b -> c * (t^pow) + b


easeOutPow :: Int -> TweenWire
easeOutPow pow = tweenWire $ \c t b ->
    let t' = t - 1
        c' = if pow `mod` 2 == 1 then c else -c
        i  = if pow `mod` 2 == 1 then 1 else -1
    in c' * ((t'^pow) + i) + b


easeInSine :: TweenWire
easeInSine = tweenWire $ \c t b ->
    let cos' = cos (t * (pi / 2))
    in -c * cos' + c + b


easeOutSine :: TweenWire
easeOutSine = tweenWire $ \c t b ->
    let cos' = cos (t * (pi / 2))
    in c * cos' + b


easeInOutSine :: TweenWire
easeInOutSine = tweenWire $ \c t b ->
    let cos' = cos (pi * t)
    in (-c / 2) * (cos' - 1) + b


easeInExpo :: TweenWire
easeInExpo = tweenWire $ \c t b ->
    let e = 10 * (t - 1)
    in c * (2**e) + b


easeOutExpo :: TweenWire
easeOutExpo = tweenWire $ \c t b ->
    let e = -10 * t
    in c * (-(2**e) + 1) + b


easeInOutExpo :: TweenWire
easeInOutExpo = easeInOut easeInExpo easeOutExpo


easeInCirc :: TweenWire
easeInCirc = tweenWire $ \c t b ->
    let s = sqrt (1 - t*t)
    in -c * (s - 1) + b


easeOutCirc :: TweenWire
easeOutCirc = tweenWire $ \c t b ->
    let t' = (t - 1)
        s  = sqrt (1 - t'*t')
    in c * s + b


easeInOutCirc :: TweenWire
easeInOutCirc = easeInOut easeInCirc easeOutCirc

-- | Time as a percentage of t seconds.
timePercentOf :: (Ord c, Monad m, Fractional c) => c -> Wire m a c
timePercentOf t = arr (\t' -> if t' > 1 then 1 else t') . (timeF / pure t)


tweenWire :: (Applicative f1, Applicative f, Real b, Num s, Monad m, Fractional c, Fractional b) => (f1 s -> Wire m a c -> f s -> t) -> s -> s -> b -> t
tweenWire f start end dur = f c t b
    where c = pure $ end - start
          t = arr realToFrac . timePercentOf dur
          b = pure start


easeInOut :: (Monad m) => TweenWire -> TweenWire -> Double -> Double -> Double -> Wire m a Double
easeInOut ein eout start end dur =
    let middle = start + (end - start) / 2
        up     = ein start middle (dur/2)
        down   = eout middle end (dur/2)
    in switchWhen' (at $ dur/2) up down


type TweenWire = (Monad m) => Double -> Double -> Double -> Wire m a Double


switchWhen :: Monad m => (b -> Bool) -> Wire m a b -> Wire m a b -> Wire m a b
switchWhen p w1 w2 = mkGen $ \dt a -> do
    Output b w1' <- stepWire w1 dt a
    return $ if p b
               then Output b w2
               else Output b (switchWhen p w1' w2)

switchWhen' :: Monad m => Wire m a (Event c) -> Wire m a b -> Wire m a b -> Wire m a b
switchWhen' sw w1 w2 = mkGen $ \dt a -> do
    Output ev sw' <- stepWire sw dt a
    case ev of
        NoEvent -> do Output b w1' <- stepWire w1 dt a
                      return $ Output b (switchWhen' sw' w1' w2)
        Event _ -> stepWire w2 dt a

cycleBetween :: Monad m => Wire m a (Event c) -> [Wire m a b] -> Wire m a b
cycleBetween sw ws = mkGen $ \dt a -> do
    Output ev sw' <- stepWire sw dt a
    let wh = head ws
        wt = tail ws
    Output b wh' <- stepWire wh dt a
    return $ case ev of
        NoEvent -> Output b (cycleBetween sw' (wh':wt))

        Event _ -> Output b (cycleBetween sw' (wt ++ [wh']))


cycleBetweenA :: Monad m => Wire m a (Event c) -> [Wire m a b] -> Wire m a b
cycleBetweenA switch wires = cycleBetweenA' switch (head wires) (tail (wires ++ [head wires]))
    where cycleBetweenA' sw wh ws = mkGen $ \dt a -> do
            Output ev sw' <- stepWire sw dt a
            Output b wh' <- stepWire wh dt a
            return $ case ev of
                NoEvent -> Output b (cycleBetweenA' sw' wh' ws)

                Event _ -> Output b (cycleBetweenA sw' ws)

