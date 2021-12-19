module App.NeuralNetworks.View.Scene exposing (scene, treeWithConfig)

import App.NeuralNetworks.View.Material as Material
import Color exposing (Color)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Tree
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene as Scene
import XYZMika.XYZ.Scene.Object as Object


type alias Config =
    { gizmos :
        { viewCone : Bool
        , seeing : Bool
        }
    }


scene : Scene.Scene Material.Name config
scene =
    Tree.singleton (Object.group "LOADING...")
        |> Scene.init
        |> Scene.withCameraPosition (vec3 0 0 20)


type alias Layer =
    { inputs : List Float
    , outputs : List Float
    , weights : List (List Float)
    , inputsWithWeights : List ( Float, List Float )
    , outputsWithWeights : List ( Float, List Float )
    }


treeWithConfig : Config -> NeuralNetwork -> Tree.Tree (Object.Object Material.Name)
treeWithConfig config neuralNetwork =
    let
        layers : List Layer
        layers =
            neuralNetwork
                |> NeuralNetwork.toLayers
                |> List.map NeuralNetwork.layerOutputs
                |> List.map (Debug.log "LAYER")

        outputs =
            layers
                |> List.reverse
                |> List.head
                |> Maybe.map .outputs
                |> Maybe.withDefault []

        gridSize =
            { x = 5
            , y = 2
            }
    in
    Tree.tree
        (Object.group "LAYERS")
        (layers
            |> List.map .inputsWithWeights
            |> List.indexedMap (layerView gridSize (List.length layers))
            |> List.append (outputs |> List.indexedMap (outputView gridSize (List.length layers)))
        )


type alias V2 =
    { x : Float, y : Float }


layerView : V2 -> Int -> Int -> List ( Float, List Float ) -> Tree.Tree (Object.Object Material.Name)
layerView gridSize count index inputs =
    let
        height =
            (List.length inputs |> toFloat) * gridSize.y - gridSize.y

        width =
            (count |> toFloat) * gridSize.x

        x =
            (toFloat index * gridSize.x) - (width / 2)

        yDiff =
            --height / 2 - gridSize.y
            -height / 2
    in
    Tree.tree
        (Object.group "LAYER"
            |> Object.withPosition (vec3 x 0 0)
        )
        [ Tree.tree
            (Object.group "LAYER_INPUTS")
            (inputs |> List.indexedMap (\i node -> neuronView gridSize yDiff i node))
        ]


neuronView : V2 -> Float -> Int -> ( Float, List Float ) -> Tree.Tree (Object.Object Material.Name)
neuronView gridSize yDiff index ( input, weights ) =
    let
        size_ =
            gridSize.y * 0.2

        y =
            toFloat index * gridSize.y + yDiff

        green =
            max 0 input

        red =
            abs (min 0 input)

        color =
            Color.rgb red green 0
                |> Color.toHsla
                |> (\hsla ->
                        Color.fromHsla { hsla | lightness = 0.5 }
                   )
    in
    Tree.tree
        (XYZMika.XYZ.Mesh.Cube.withBounds
            ( vec3 -size_ -size_ -size_
            , vec3 size_ size_ size_
            )
            |> Object.initWithTriangles
            |> Object.withColor color
            |> Object.withPosition (vec3 0 y 0)
            |> Object.withMaterialName Material.Color
        )
        (weights |> List.indexedMap (weightView { yDiffParent = yDiff, count = List.length weights, neuronIndex = index, gridSize = gridSize, size = size_ * 0.05 }))


weightView : { yDiffParent : Float, count : Int, neuronIndex : Int, gridSize : V2, size : Float } -> Int -> Float -> Tree.Tree (Object.Object Material.Name)
weightView { yDiffParent, count, neuronIndex, gridSize, size } index weight =
    let
        height =
            (count |> toFloat) * gridSize.y - gridSize.y

        yDiff =
            -height / 2

        dIndex =
            index - neuronIndex

        outputY =
            toFloat dIndex * gridSize.y - yDiffParent + yDiff

        ( length, a ) =
            toPolar ( gridSize.x, outputY )

        green =
            max 0 weight

        red =
            abs (min 0 weight)

        color =
            Color.rgb red green 0
    in
    Tree.singleton
        (XYZMika.XYZ.Mesh.Cube.withBounds
            ( vec3 0 -size -size
            , vec3 length size size
            )
            |> Object.initWithTriangles
            |> Object.withColor color
            |> Object.withRotation (Mat4.makeRotate a (vec3 0 0 1))
            |> Object.withMaterialName Material.Color
        )


outputView : V2 -> Int -> Int -> Float -> Tree.Tree (Object.Object Material.Name)
outputView gridSize size index output =
    let
        size_ =
            gridSize.y * 0.2

        height =
            ((size - 1) |> toFloat) * gridSize.y

        y =
            toFloat index * gridSize.y - (height / 2)

        x =
            toFloat size * gridSize.x / 2

        green =
            max 0 output

        red =
            abs (min 0 output)

        color =
            Color.rgb red green 0
                |> Color.toHsla
                |> (\hsla ->
                        Color.fromHsla { hsla | lightness = 0.5 }
                   )
    in
    Tree.singleton
        (XYZMika.XYZ.Mesh.Cube.withBounds
            ( vec3 -size_ -size_ -size_
            , vec3 size_ size_ size_
            )
            |> Object.initWithTriangles
            |> Object.withColor color
            |> Object.withPosition (vec3 x y 0)
            |> Object.withMaterialName Material.Color
        )
