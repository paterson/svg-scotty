# CS4012 Assignment 1
Niall Paterson 12305503

### Installation and initiation

    git clone https://github.com/paterson/svg-scotty
    stack install
    stack exec svg-scotty-exe
    open http://localhost:3002

There is sample shapes included in the textarea already.

### Documentation

#### Drawing

A drawing is in the following format:

    [(Transform,Shape, Stylesheet)]

An Example drawing is the following:

    [
    (Scale (Vector 0.5 0.5), Square, [Fill Blue, Stroke Red, Outline 10, Height 70, Width 70]),
    (Translate (Vector 170 160), Circle, [Fill (Hex "#666666"), Stroke Cyan, Outline 20, Radius 40]),
    (Compose (Rotate 45) (Scale (Vector 0.75 0.75)), Square, [Fill Magenta, Stroke (RGB 255 255 0), Outline 20, Height 30, Width 30, X 100, Y 50])
    ]

#### Colours

    Black
    White
    Red
    Green
    Yellow
    Blue
    Orange
    Magenta
    Cyan

You are also able to define colours by RGB and Hex:

    (RGB 255 0 255)
    (Hex "#F5F5F5")

#### Styles

    Stroke Color
    Fill Color
    Outline Float
    X Float
    Y Float
    Width Float
    Height Float
    CenterX Float
    CenterY Float
    Radius Float

A stylesheet is made up of a list of styles.

#### Transforms

    Identity
    Translate Vector
    Scale Vector
    Compose Transform Transform
    Rotate Double

There is also a helper function for compose:

    t0 <+> t1 = Compose t0 t1
