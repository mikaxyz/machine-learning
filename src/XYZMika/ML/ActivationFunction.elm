module XYZMika.ML.ActivationFunction exposing
    ( ActivationFunction(..)
    , dFunc
    , func
    )


type ActivationFunction
    = Sigmoid
    | Tanh


func : ActivationFunction -> (Float -> Float)
func x =
    case x of
        Sigmoid ->
            sigmoid

        Tanh ->
            tanh


dFunc : ActivationFunction -> (Float -> Float)
dFunc x =
    case x of
        Sigmoid ->
            dSigmoid

        Tanh ->
            dTanh


tanh : Float -> Float
tanh x =
    let
        t =
            ((e ^ x) - (e ^ -x)) / ((e ^ x) + (e ^ -x))
    in
    (t + 1) / 2


dTanh : Float -> Float
dTanh y =
    -- y is in [0,1] after mapping; underlying tanh derivative is (1 - t^2)
    -- where y = (t + 1) / 2 and t = tanh(z), thus t = 2y - 1
    -- dy/dz = (1/2) * (1 - t^2)
    let
        t =
            2 * y - 1
    in
    0.5 * (1 - (t * t))


sigmoid : Float -> Float
sigmoid x =
    1 / (1 + e ^ -x)


dSigmoid : Float -> Float
dSigmoid y =
    y * (1 - y)
