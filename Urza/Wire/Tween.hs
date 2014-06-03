{-# LANGUAGE Rank2Types #-}
module Urza.Wire.Tween where

import Prelude hiding ((.), id)
import FRP.Netwire


type TweenWire = (HasTime t s, Monoid e, Monad m, Fractional b, Fractional t) => b -> b -> t -> Wire s e m a b


type TweenWireF = (HasTime t s, Monoid e, Monad m, Floating b, Fractional b, Fractional t) => b -> b -> t -> Wire s e m a b


timePercent :: (HasTime b s, Monoid e, Monad m, Fractional b, Fractional c) => b -> Wire s e m a c
timePercent t = fmap realToFrac $ (when (<=1) . (time / pure t)) <|> 1


tweenWire :: (HasTime b s, Monoid e, Applicative f1, Applicative f, Num s1, Monad m, Fractional c, Fractional b) => (f1 s1 -> Wire s e m a c -> f s1 -> t) -> s1 -> s1 -> b -> t
tweenWire f start end dur = f c t b
    where c = pure $ end - start
          t = timePercent dur
          b = pure start


easeInOut :: (HasTime t s, Monoid e, Monad m, Fractional b, Fractional t) => TweenWire -> TweenWire -> b -> b -> t -> Wire s e m a b
easeInOut ein eout start end dur =
    let middle = start + (end - start) / 2
    in for (dur/2) . ein start middle (dur/2)
           --> eout middle end (dur/2)


easeInOutF :: (HasTime t s, Monoid e, Monad m, Fractional b, Fractional t, Floating b) => TweenWireF -> TweenWireF -> b -> b -> t -> Wire s e m a b
easeInOutF ein eout start end dur =
    let middle = start + (end - start) / 2
    in for (dur/2) . ein start middle (dur/2)
           --> eout middle end (dur/2)


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


easeInSine :: TweenWireF
easeInSine = tweenWire $ \c t b ->
    let cos' = cos (t * (pi / 2))
    in -c * cos' + c + b


easeOutSine :: TweenWireF
easeOutSine = tweenWire $ \c t b ->
    let cos' = cos (t * (pi / 2))
    in c * cos' + b


easeInOutSine :: TweenWireF
easeInOutSine = tweenWire $ \c t b ->
    let cos' = cos (pi * t)
    in (-c / 2) * (cos' - 1) + b


easeInExpo :: TweenWireF
easeInExpo = tweenWire $ \c t b ->
    let e = 10 * (t - 1)
    in c * (2**e) + b


easeOutExpo :: TweenWireF
easeOutExpo = tweenWire $ \c t b ->
    let e = -10 * t
    in c * (-(2**e) + 1) + b


easeInOutExpo :: TweenWireF
easeInOutExpo = easeInOutF easeInExpo easeOutExpo


easeInCirc :: TweenWireF
easeInCirc = tweenWire $ \c t b ->
    let s = sqrt (1 - t*t)
    in -c * (s - 1) + b


easeOutCirc :: TweenWireF
easeOutCirc = tweenWire $ \c t b ->
    let t' = (t - 1)
        s  = sqrt (1 - t'*t')
    in c * s + b


easeInOutCirc :: TweenWireF
easeInOutCirc = easeInOutF easeInCirc easeOutCirc
