module App.NeuralNetworks.View.Page exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Api.Model as Api
import Api.Models
import App.NeuralNetworks.View.Dragon as Dragon exposing (Dragon)
import App.NeuralNetworks.View.Material as Material
import App.NeuralNetworks.View.Scene as Scene
import Html exposing (Html, form, progress, section, text)
import Html.Attributes exposing (height, id, width)
import Http
import Image exposing (Image)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (vec3)
import RemoteData exposing (RemoteData)
import Task
import WebGL
import WebGL.Texture exposing (Texture)
import Worker.Task
import XYZMika.ML.NeuralNetwork exposing (NeuralNetwork, TrainingData)
import XYZMika.XYZ.Material
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene as XYZScene exposing (Scene)
import XYZMika.XYZ.Scene.Camera as Camera
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Options as SceneOptions
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


config =
    { simulationSize = 40
    , scene =
        { gizmos =
            { viewCone = False
            , seeing = True
            }
        }
    }


type Msg
    = OnTextureLoad (Result WebGL.Texture.Error Texture)
    | DragonMsg Dragon.Msg
    | OnDrag Vec2
      --
    | OnChange Form
    | OnNeuralNetwork (Result Http.Error Api.Model)
    | WorkerTaskCompleted Worker.Task.Result
    | WorkerTaskProgress (Result String Worker.Task.Progress)
    | OnSubmit
    | OnSaved
        { title : String
        , neuralNetwork : NeuralNetwork
        }
        (Result Http.Error ())


type Form
    = Inputs String


type alias Model =
    { neuralNetwork : RemoteData String NeuralNetwork
    , trainingData : List TrainingData
    , inputs : String
    , scene : Scene Material.Name {}
    , sceneOptions : SceneOptions.Options
    , defaultTexture : Result WebGL.Texture.Error Texture
    , dragon : Dragon
    }


init : Int -> ( Model, Cmd Msg )
init id =
    let
        createDefaultTexture : Task.Task WebGL.Texture.Error Texture
        createDefaultTexture =
            Image.fromList2d
                [ List.repeat 1 0xFFFF
                ]
                |> Image.toPngUrl
                |> WebGL.Texture.load
    in
    ( { neuralNetwork = RemoteData.Loading
      , trainingData = []
      , inputs = ""
      , scene = Scene.scene
      , sceneOptions =
            SceneOptions.create
                |> SceneOptions.toggle SceneOptions.showGridYOption
      , defaultTexture = Err WebGL.Texture.LoadError
      , dragon = Dragon.init
      }
    , Cmd.batch
        [ Api.Models.getById id OnNeuralNetwork
        , Task.attempt OnTextureLoad createDefaultTexture
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Worker.Task.onCompleted WorkerTaskCompleted
        , Worker.Task.onProgress WorkerTaskProgress
        , Dragon.subscriptions model.dragon |> Sub.map DragonMsg
        ]


update : { tagger : Msg -> msg } -> Msg -> Model -> ( Model, Cmd msg )
update { tagger } msg model =
    case msg of
        OnTextureLoad result ->
            ( { model | defaultTexture = result }, Cmd.none )

        DragonMsg msg_ ->
            Dragon.update { tagger = DragonMsg, onDragUpdate = OnDrag } msg_ model.dragon
                |> Tuple.mapBoth
                    (\dragon -> { model | dragon = dragon })
                    (Cmd.map tagger)

        OnDrag pos ->
            ( { model
                | scene =
                    model.scene
                        |> XYZScene.withCameraMap
                            (\camera ->
                                camera
                                    |> Camera.withOrbitY -(Vec2.getX pos / 100)
                                    |> Camera.withPositionMap
                                        (\position ->
                                            position
                                                |> Vec3.setY
                                                    (Vec3.getY position + (Vec2.getY pos / 200))
                                        )
                            )
              }
            , Cmd.none
            )

        OnChange form ->
            ( case form of
                Inputs x ->
                    { model | inputs = x }
            , Cmd.none
            )

        OnSubmit ->
            case model.neuralNetwork of
                RemoteData.Loaded neuralNetwork ->
                    ( { model | neuralNetwork = RemoteData.Progress 0 }
                    , Worker.Task.TrainNeuralNetwork 100 neuralNetwork
                        |> Worker.Task.start
                    )

                _ ->
                    ( model, Cmd.none )

        OnSaved { title, neuralNetwork } result ->
            ( model
            , Cmd.none
            )

        OnNeuralNetwork result ->
            ( case result of
                Ok model_ ->
                    { model
                        | neuralNetwork = RemoteData.Loaded model_.data
                        , scene =
                            model.scene |> XYZScene.map (\_ -> Scene.treeWithConfig config.scene model_.data)
                    }

                Err _ ->
                    { model | neuralNetwork = RemoteData.Error "Could not load model" }
            , Cmd.none
            )

        WorkerTaskCompleted result ->
            case result of
                Worker.Task.TrainedNeuralNetwork neuralNetwork ->
                    ( { model | neuralNetwork = RemoteData.Loaded neuralNetwork }
                    , Cmd.none
                    )

                Worker.Task.UnknownResult ->
                    ( model, Cmd.none )

        WorkerTaskProgress (Ok progress) ->
            let
                _ =
                    Debug.log "WorkerTaskProgress" progress
            in
            ( { model | neuralNetwork = RemoteData.Progress progress.complete }
            , Cmd.none
            )

        WorkerTaskProgress (Err _) ->
            ( model
            , Cmd.none
            )


viewport =
    { width = 1600
    , height = 1200
    }


view : Model -> Html Msg
view model =
    section []
        [ case ( model.defaultTexture, model.neuralNetwork ) of
            ( Ok defaultTexture, RemoteData.Loaded x ) ->
                WebGL.toHtml
                    [ width viewport.width
                    , height viewport.height
                    , id "viewport"
                    ]
                    (XYZScene.render
                        defaultTexture
                        [ Light.directional (vec3 1 3 3)
                        , Light.pointLight (vec3 2 3 1)
                        ]
                        model.sceneOptions
                        viewport
                        (Dragon.currentVec2 model.dragon)
                        0
                        Nothing
                        (\_ -> Nothing)
                        model.scene
                        renderer
                    )

            _ ->
                text ""
        ]


renderer :
    Maybe Material.Name
    -> XYZMika.XYZ.Material.Options
    -> Texture
    -> Uniforms u
    -> Object Material.Name
    -> WebGL.Entity
renderer name =
    case name of
        Just materialName ->
            Material.renderer materialName

        Nothing ->
            XYZMika.XYZ.Material.Simple.renderer
