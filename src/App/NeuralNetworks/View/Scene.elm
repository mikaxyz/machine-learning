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
        |> Scene.withCameraPosition (vec3 0 0 1)


type alias Layer =
    { inputs : List Float
    , outputs : List Float
    , weights : List (List Float)
    , data : List ( Float, List Float )
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
    in
    Tree.tree
        (Object.group "LAYERS")
        (layers |> List.indexedMap (layerView (List.length layers)))


layerView : Int -> Int -> Layer -> Tree.Tree (Object.Object Material.Name)
layerView count index layer =
    let
        gridSize =
            0.05

        height =
            (List.length layer.weights |> toFloat) * (gridSize * 2)

        width =
            (1 + count |> toFloat) * (3 * gridSize)

        x =
            (toFloat index * (3 * gridSize)) * 2 - (width / 2)

        y =
            height / 2 - gridSize
    in
    Tree.tree
        (Object.group "LAYER"
            |> Object.withPosition (vec3 x -y 0)
        )
        --[ Tree.tree
        --    (Object.group "LAYER_OUTPUTS")
        --    (layer.outputs |> List.indexedMap (\i node -> outputView size i node))
        --, Tree.tree
        --    (Object.group "LAYER_INPUTS")
        --    (layer.inputs |> List.indexedMap (\i node -> inputView size i node))
        --]
        [ Tree.tree
            (Object.group "LAYER_OUTPUTS")
            (layer.data |> List.indexedMap (\i node -> dataView gridSize i node))
        ]


dataView : Float -> Int -> ( Float, List Float ) -> Tree.Tree (Object.Object Material.Name)
dataView gridSize index ( output, weights ) =
    let
        size_ =
            gridSize * 0.5

        y =
            (toFloat index * gridSize) * 2

        green =
            max 0 output

        red =
            abs (min 0 output)

        color =
            Color.rgb red green 0

        --|> Color.toHsla
        --|> (\hsla ->
        --        Color.fromHsla { hsla | lightness = 0.5 - green - red }
        --   )
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
        [ weightsView y index gridSize (List.length weights) weights ]


weightsView : Float -> Int -> Float -> Int -> List Float -> Tree.Tree (Object.Object Material.Name)
weightsView y index gridSize count weights =
    let
        --size =
        --    0.05
        --height =
        --    (List.length weights |> toFloat) * gridSize
        --height =
        --    (List.length weights |> toFloat) * (gridSize * 2)
        --width =
        --    (1 + count |> toFloat) * (3 * size)
        x =
            --(toFloat index * (3 * size)) * 2 - (width / 2)
            -0.05 * toFloat (index + 1)

        --y =
        --    (height / 2) - gridSize
    in
    Tree.tree
        (Object.group "LAYER_WEIGHTS"
         --|> Object.withPosition (vec3 x -y 0)
         --|> Object.withPosition (vec3 x -y 0)
        )
        --[ Tree.tree
        --    (Object.group "LAYER_OUTPUTS")
        --    (weights |> List.indexedMap (\i node -> weightView y (List.length weights) gridSize i node))
        --]
        (weights |> List.indexedMap (\i node -> weightView y (List.length weights) gridSize i node))


weightView : Float -> Int -> Float -> Int -> Float -> Tree.Tree (Object.Object Material.Name)
weightView nodeY count gridSize index node =
    let
        size_ =
            gridSize * 0.02

        height =
            (count |> toFloat) * (gridSize * 2)

        y =
            ((toFloat index * gridSize) * 2)
                - (height / 2)
                + nodeY

        green =
            max 0 node

        red =
            abs (min 0 node)

        color =
            Color.rgb red green 0

        --pointAt : Vec3
        --pointAt =
        --    vec3 -gridSize y 0
        --length =
        --    Vec3.length pointAt
        --a =
        --    Vec3.dot
        --        (vec3 0 0 1)
        --        pointAt
        ( length, a ) =
            toPolar ( gridSize * 6, y )
    in
    Tree.singleton
        (XYZMika.XYZ.Mesh.Cube.withBounds
            ( vec3 0 -size_ -size_
            , vec3 -length size_ size_
            )
            |> Object.initWithTriangles
            |> Object.withColor color
            |> Object.withRotation (Mat4.makeRotate a (vec3 0 0 1))
            |> Object.withPosition (vec3 0 0 (gridSize * 0.6))
            |> Object.withMaterialName Material.Color
        )


inputView : Float -> Int -> Float -> Tree.Tree (Object.Object Material.Name)
inputView size index node =
    let
        size_ =
            size * 0.2

        y =
            (toFloat index * size) * 2
    in
    Tree.singleton
        (XYZMika.XYZ.Mesh.Cube.withBounds
            ( vec3 -size_ -size_ -size_
            , vec3 size_ size_ size_
            )
            |> Object.initWithTriangles
            |> Object.withColor Color.white
            |> Object.withPosition (vec3 -size y 0)
            |> Object.withMaterialName Material.Color
        )


outputView : Float -> Int -> Float -> Tree.Tree (Object.Object Material.Name)
outputView size index node =
    let
        size_ =
            size * 0.5

        y =
            (toFloat index * size) * 2

        green =
            max 0 node

        red =
            abs (min 0 node)

        color =
            Color.rgb red green 0

        --|> Color.toHsla
        --|> (\hsla ->
        --        Color.fromHsla { hsla | lightness = 0.5 - green - red }
        --   )
    in
    Tree.singleton
        (XYZMika.XYZ.Mesh.Cube.withBounds
            ( vec3 -size_ -size_ -size_
            , vec3 size_ size_ size_
            )
            |> Object.initWithTriangles
            |> Object.withColor color
            |> Object.withPosition (vec3 0 y 0)
            |> Object.withMaterialName Material.Color
        )
