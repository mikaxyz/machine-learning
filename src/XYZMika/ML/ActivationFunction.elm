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
    ((e ^ x) - (e ^ -x)) / ((e ^ x) + (e ^ -x))


dTanh : Float -> Float
dTanh y =
    1 - (y * y)


sigmoid : Float -> Float
sigmoid x =
    1 / (1 + e ^ -x)


dSigmoid : Float -> Float
dSigmoid y =
    y * (1 - y)
