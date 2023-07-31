> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts          #-}
> {-# LANGUAGE TypeFamilies              #-}
>
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
>
> myCircle :: Diagram B
> myCircle = circle 1 # translate (r2 (0.5, 0.3)) # showOrigin
>
> main = mainWith myCircle