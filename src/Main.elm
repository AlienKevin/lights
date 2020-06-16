port module Main exposing (..)

import Browser
import Color exposing (Color)
import Color.Convert
import Color.Manipulate
import ColorPicker
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder, float)
import Json.Decode.Field as Field
import Json.Encode as Encode
import Round
import TypedSvg as Svg
import TypedSvg.Attributes as Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), px)


port generateModelPort :
    { sparsity : Float
    , step : Float
    , scheme : Encode.Value
    , style : Encode.Value
    , width : Float
    , height : Float
    , skew : Encode.Value
    , sizeRange : Range
    }
    -> Cmd msg


port receivedModelPort : (Encode.Value -> msg) -> Sub msg


port downloadModelPort : () -> Cmd msg


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { lights : List Light
    , sparsity : Float
    , step : Float
    , background : Color
    , scheme : Scheme
    , style : Style
    , width : Float
    , height : Float
    , skew : Range
    , sizeRange : Range
    , gradient : Gradient
    , backgroundColorPicker : ColorPicker.State
    }


decodeModel : Decoder Model
decodeModel =
    Field.require "lights" (Decode.list decodeLight) <|
        \lights ->
            Field.require "sparsity" Decode.float <|
                \sparsity ->
                    Field.require "step" Decode.float <|
                        \step ->
                            Field.require "background" decodeColor <|
                                \background ->
                                    Field.require "scheme" decodeScheme <|
                                        \scheme ->
                                            Field.require "style" decodeStyle <|
                                                \style ->
                                                    Field.require "width" Decode.float <|
                                                        \width ->
                                                            Field.require "height" Decode.float <|
                                                                \height ->
                                                                    Field.require "skew"
                                                                        decodeRange
                                                                    <|
                                                                        \skew ->
                                                                            Field.require "sizeRange"
                                                                                decodeRange
                                                                            <|
                                                                                \sizeRange ->
                                                                                    Field.require "gradient"
                                                                                        decodeGradient
                                                                                    <|
                                                                                        \gradient ->
                                                                                            Decode.succeed
                                                                                                { lights = lights
                                                                                                , sparsity = sparsity
                                                                                                , step = step
                                                                                                , background = background
                                                                                                , scheme = scheme
                                                                                                , style = style
                                                                                                , width = width
                                                                                                , height = height
                                                                                                , skew = skew
                                                                                                , sizeRange = sizeRange
                                                                                                , gradient = gradient
                                                                                                , backgroundColorPicker = ColorPicker.empty
                                                                                                }


extractLights : Decoder (List Light)
extractLights =
    Field.require "lights" (Decode.list decodeLight) <|
        \lights ->
            Decode.succeed lights


type Gradient
    = UseGradient
        { innerOffset : Int
        , outerOffset : Int
        , outerAlphaScale : Float
        }
    | NoGradient


decodeGradient : Decoder Gradient
decodeGradient =
    Decode.string
        |> (Decode.andThen <|
                \name ->
                    case name of
                        "UseGradient" ->
                            Field.require "innerOffset" Decode.int <|
                                \innerOffset ->
                                    Field.require "outerOffset" Decode.int <|
                                        \outerOffset ->
                                            Field.require "outerAlphaScale" Decode.float <|
                                                \outerAlphaScale ->
                                                    Decode.succeed <|
                                                        UseGradient
                                                            { innerOffset =
                                                                innerOffset
                                                            , outerOffset =
                                                                outerOffset
                                                            , outerAlphaScale =
                                                                outerAlphaScale
                                                            }

                        "NoGradient" ->
                            Decode.succeed NoGradient

                        _ ->
                            Decode.fail "Invailde Gradient"
           )


type alias Light =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , color : Color
    }


decodeLight : Decoder Light
decodeLight =
    Field.require "x" Decode.float <|
        \x ->
            Field.require "y" Decode.float <|
                \y ->
                    Field.require "width" Decode.float <|
                        \width ->
                            Field.require "height" Decode.float <|
                                \height ->
                                    Field.require "color" decodeColor <|
                                        \color ->
                                            Decode.succeed
                                                { x = x
                                                , y = y
                                                , width = width
                                                , height = height
                                                , color = color
                                                }


decodeColor : Decoder Color
decodeColor =
    Decode.string
        |> Decode.andThen
            (\str ->
                case Color.Convert.hexToColor str of
                    Ok color ->
                        Decode.succeed color

                    Err err ->
                        Decode.fail err
            )


type Style
    = DefaultStyle
    | PastelStyle
    | SoftStyle
    | HardStyle
    | LightStyle
    | PaleStyle


encodeStyle : Style -> Encode.Value
encodeStyle style =
    Encode.string <| styleToString style


styleToString : Style -> String
styleToString style =
    case style of
        DefaultStyle ->
            "default"

        PastelStyle ->
            "pastel"

        SoftStyle ->
            "soft"

        HardStyle ->
            "hard"

        LightStyle ->
            "light"

        PaleStyle ->
            "pale"


decodeStyle : Decoder Style
decodeStyle =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "pastel" ->
                        Decode.succeed PastelStyle

                    "soft" ->
                        Decode.succeed SoftStyle

                    "hard" ->
                        Decode.succeed HardStyle

                    "light" ->
                        Decode.succeed LightStyle

                    "pale" ->
                        Decode.succeed PaleStyle

                    "default" ->
                        Decode.succeed DefaultStyle

                    _ ->
                        Decode.fail "Invalid Style"
            )


type Scheme
    = MonoScheme
    | ContrastScheme
    | TriadeScheme
        { distance : Float
        }
    | TetradeScheme
        { distance : Float
        }
    | AnalogicScheme
        { distance : Float
        , complemented : Bool
        }


encodeScheme : Scheme -> Encode.Value
encodeScheme scheme =
    case scheme of
        MonoScheme ->
            Encode.object
                [ ( "scheme", Encode.string "mono" ) ]

        ContrastScheme ->
            Encode.object
                [ ( "scheme", Encode.string "contrast" ) ]

        TriadeScheme { distance } ->
            Encode.object
                [ ( "scheme", Encode.string "triade" )
                , ( "distance", Encode.float distance )
                ]

        TetradeScheme { distance } ->
            Encode.object
                [ ( "scheme", Encode.string "tetrade" )
                , ( "distance", Encode.float distance )
                ]

        AnalogicScheme { distance, complemented } ->
            Encode.object
                [ ( "scheme", Encode.string "analogic" )
                , ( "distance", Encode.float distance )
                , ( "complemented", Encode.bool complemented )
                ]


decodeScheme : Decoder Scheme
decodeScheme =
    Field.require "scheme" Decode.string <|
        \scheme ->
            case scheme of
                "mono" ->
                    Decode.succeed MonoScheme

                "contrast" ->
                    Decode.succeed ContrastScheme

                "triade" ->
                    Field.require "distance" Decode.float <|
                        \distance ->
                            Decode.succeed <|
                                TriadeScheme { distance = distance }

                "tetrade" ->
                    Field.require "distance" Decode.float <|
                        \distance ->
                            Decode.succeed <|
                                TetradeScheme { distance = distance }

                "analogic" ->
                    Field.require "distance" Decode.float <|
                        \distance ->
                            Field.require "complemented" Decode.bool <|
                                \complemented ->
                                    Decode.succeed <|
                                        AnalogicScheme
                                            { distance = distance
                                            , complemented = complemented
                                            }

                _ ->
                    Decode.fail "Invalid Scheme"


schemeToString : Scheme -> String
schemeToString scheme =
    case scheme of
        MonoScheme ->
            "mono"

        ContrastScheme ->
            "contrast"

        TriadeScheme _ ->
            "triade"

        TetradeScheme _ ->
            "tetrade"

        AnalogicScheme _ ->
            "analogic"


type alias Range =
    ( Float, Float )


encodeRange : Range -> Encode.Value
encodeRange ( start, end ) =
    Encode.list identity [ Encode.float start, Encode.float end ]


decodeRange : Decoder Range
decodeRange =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { lights = []
            , step = 2
            , sparsity = 0.6
            , scheme = MonoScheme
            , style = DefaultStyle
            , background = Color.white
            , width = 600
            , height = 600
            , skew = ( 0.8, 1 )
            , sizeRange = ( 20, 100 )
            , gradient =
                UseGradient
                    { innerOffset = 10
                    , outerOffset = 100
                    , outerAlphaScale = -0.5
                    }
            , backgroundColorPicker = ColorPicker.empty
            }
    in
    ( model
    , generateModel model
    )


type Msg
    = ReceivedModel Encode.Value
    | ChangedScheme Scheme
    | ChangedStyle Style
    | ChangedGradient Gradient
    | ChangedWidth Float
    | ChangedHeight Float
    | ChangeStep Float
    | ChangeSparsity Float
    | ChangedSkew Range
    | ChangedSizeRange Range
    | ChangedBackgroundColorPicker ColorPicker.Msg
    | GenerateModel
    | DownloadModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedModel modelValue ->
            receivedModel modelValue model

        ChangedScheme newScheme ->
            changedScheme newScheme model

        ChangedStyle newStyle ->
            changedStyle newStyle model

        ChangedGradient newGradient ->
            changedGradient newGradient model

        ChangedWidth newWidth ->
            changedWidth newWidth model

        ChangedHeight newHeight ->
            changedHeight newHeight model

        ChangeStep newStep ->
            changeStep newStep model

        ChangeSparsity newSparsity ->
            changeSparsity newSparsity model

        ChangedSkew newSkew ->
            changedSkew newSkew model

        ChangedSizeRange newSizeRange ->
            changedSizeRange newSizeRange model

        ChangedBackgroundColorPicker colorPickerMsg ->
            changedBackgroundColorPicker colorPickerMsg model

        GenerateModel ->
            ( model, generateModel model )

        DownloadModel ->
            ( model, downloadModelPort () )


changedGradient : Gradient -> Model -> ( Model, Cmd Msg )
changedGradient newGradient model =
    ( { model
        | gradient =
            case newGradient of
                UseGradient { innerOffset, outerOffset, outerAlphaScale } ->
                    let
                        approachOuterOffset =
                            UseGradient
                                { innerOffset = outerOffset
                                , outerOffset = outerOffset
                                , outerAlphaScale = outerAlphaScale
                                }

                        approachInnerOffset =
                            UseGradient
                                { innerOffset = innerOffset
                                , outerOffset = innerOffset
                                , outerAlphaScale = outerAlphaScale
                                }
                    in
                    if innerOffset >= outerOffset then
                        case model.gradient of
                            NoGradient ->
                                approachOuterOffset

                            UseGradient oldGradient ->
                                if oldGradient.outerOffset > outerOffset then
                                    approachInnerOffset

                                else
                                    approachOuterOffset

                    else
                        newGradient

                NoGradient ->
                    newGradient
      }
    , Cmd.none
    )


changedBackgroundColorPicker : ColorPicker.Msg -> Model -> ( Model, Cmd Msg )
changedBackgroundColorPicker msg model =
    let
        ( newColorPicker, newBackground ) =
            ColorPicker.update msg model.background model.backgroundColorPicker
    in
    ( { model
        | backgroundColorPicker = newColorPicker
        , background = newBackground |> Maybe.withDefault model.background
      }
    , Cmd.none
    )


changeSparsity : Float -> Model -> ( Model, Cmd Msg )
changeSparsity newSparsity model =
    ( { model
        | sparsity =
            newSparsity
      }
    , generateModel model
    )


changedSizeRange : Range -> Model -> ( Model, Cmd Msg )
changedSizeRange ( newMin, newMax ) model =
    let
        approachMin =
            ( newMin, newMin )

        approachMax =
            ( newMax, newMax )

        ( _, oldMax ) =
            model.sizeRange
    in
    ( { model
        | sizeRange =
            if newMin >= newMax then
                if oldMax > newMax then
                    approachMin

                else
                    approachMax

            else
                ( newMin, newMax )
      }
    , generateModel model
    )


changedSkew : Range -> Model -> ( Model, Cmd Msg )
changedSkew ( newMin, newMax ) model =
    let
        approachMin =
            ( newMin, newMin )

        approachMax =
            ( newMax, newMax )

        ( _, oldMax ) =
            model.skew
    in
    ( { model
        | skew =
            if newMin >= newMax then
                if oldMax > newMax then
                    approachMin

                else
                    approachMax

            else
                ( newMin, newMax )
      }
    , generateModel model
    )


changeStep : Float -> Model -> ( Model, Cmd Msg )
changeStep newStep model =
    ( { model
        | step =
            newStep
      }
    , generateModel model
    )


changedHeight : Float -> Model -> ( Model, Cmd Msg )
changedHeight newHeight model =
    ( { model
        | height =
            newHeight
      }
    , Cmd.none
    )


changedWidth : Float -> Model -> ( Model, Cmd Msg )
changedWidth newWidth model =
    ( { model
        | width =
            newWidth
      }
    , Cmd.none
    )


changedStyle : Style -> Model -> ( Model, Cmd Msg )
changedStyle newStyle model =
    let
        newModel =
            { model
                | style =
                    newStyle
            }
    in
    ( newModel
    , generateModel newModel
    )


changedScheme : Scheme -> Model -> ( Model, Cmd Msg )
changedScheme newScheme model =
    let
        newModel =
            { model
                | scheme =
                    newScheme
            }
    in
    ( newModel
    , generateModel newModel
    )


receivedModel : Encode.Value -> Model -> ( Model, Cmd Msg )
receivedModel modelValue model =
    ( Result.withDefault model <|
        Result.map
            (\lights ->
                { model
                    | lights =
                        lights
                }
            )
        <|
            Decode.decodeValue
                extractLights
                modelValue
    , Cmd.none
    )


generateModel : Model -> Cmd msg
generateModel model =
    generateModelPort
        { sparsity =
            model.sparsity
        , step =
            model.step
        , scheme =
            encodeScheme model.scheme
        , style =
            encodeStyle model.style
        , width =
            model.width
        , height =
            model.height
        , skew =
            encodeRange model.skew
        , sizeRange =
            model.sizeRange
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    receivedModelPort ReceivedModel


view : Model -> Html Msg
view model =
    E.layout
        [ Font.size 16
        , E.width E.fill
        , E.height E.fill
        ]
    <|
        let
            lights =
                case model.gradient of
                    NoGradient ->
                        List.map
                            viewLight
                            model.lights

                    UseGradient gradient ->
                        List.indexedMap
                            (viewLightWithGradient model gradient)
                            model.lights
        in
        E.row
            [ E.width E.fill
            , E.height E.fill
            ]
            [ viewControlPanel model
            , E.el
                [ E.htmlAttribute <| Html.Attributes.style "margin-left" "300px"
                , E.htmlAttribute <| Html.Attributes.id "model"
                ]
              <|
                E.html <|
                    Svg.svg
                        [ Attributes.width (px model.width)
                        , Attributes.height (px model.height)
                        , Attributes.viewBox 0 0 model.width model.height
                        ]
                    <|
                        Svg.rect
                            [ Attributes.fill (Paint model.background)
                            , Attributes.width (px model.width)
                            , Attributes.height (px model.height)
                            ]
                            []
                            :: lights
            ]


viewLightWithGradient :
    Model
    ->
        { innerOffset : Int
        , outerOffset : Int
        , outerAlphaScale : Float
        }
    -> Int
    -> Light
    -> Svg Msg
viewLightWithGradient model { innerOffset, outerOffset, outerAlphaScale } index { x, y, width, height, color } =
    let
        gradientId =
            "light-gradient-" ++ String.fromInt index
    in
    Svg.g []
        [ Svg.defs []
            [ Svg.radialGradient
                [ Attributes.id <| gradientId ]
                [ Svg.stop
                    [ Attributes.offset <| String.fromInt innerOffset ++ "%"
                    , Attributes.stopColor <| Color.toCssString color
                    ]
                    []
                , Svg.stop
                    [ Attributes.offset <| String.fromInt outerOffset ++ "%"
                    , Attributes.stopColor <|
                        Color.toCssString <|
                            Color.Manipulate.scaleRgb
                                { redScale = 0
                                , greenScale = 0
                                , blueScale = 0
                                , alphaScale = outerAlphaScale
                                }
                            <|
                                model.background
                    ]
                    []
                ]
            ]
        , Svg.ellipse
            [ Attributes.cx <| px x
            , Attributes.cy <| px y
            , Attributes.rx <| px (width / 2)
            , Attributes.ry <| px (height / 2)
            , Attributes.fill <| Reference gradientId
            ]
            []
        ]


viewLight : Light -> Svg Msg
viewLight { x, y, width, height, color } =
    Svg.ellipse
        [ Attributes.cx <| px x
        , Attributes.cy <| px y
        , Attributes.rx <| px (width / 2)
        , Attributes.ry <| px (height / 2)
        , Attributes.fill <| Paint color
        ]
        []


viewControlPanel : Model -> Element Msg
viewControlPanel model =
    E.column
        [ E.alignTop
        , E.padding 10
        ]
        [ E.column
            [ E.alignTop
            , E.padding 10
            , E.scrollbarY
            , E.htmlAttribute <| Html.Attributes.style "height" "90vh"
            , E.htmlAttribute <| Html.Attributes.style "position" "fixed"
            , E.htmlAttribute <| Html.Attributes.style "top" "0"
            ]
            [ h1 "Controls"
            , h2 "Scheme"
            , viewSchemeSelector model
            , h2 "Style"
            , viewStyleSelector model
            , h2 "Gradient"
            , viewGradientSelector model
            , h2 "Background"
            , viewBackgroundSelector model
            , h2 "Dimensions"
            , viewWidthSelector model
            , viewHeightSelector model
            , h2 "Step Size"
            , viewStepSelector model
            , h2 "Sparsity"
            , viewSparsitySelector model
            , h2 "Skew"
            , viewSkewSelector model
            , h2 "Size Range"
            , viewSizeRangeSelector model
            ]
        , E.row
            [ E.htmlAttribute <| Html.Attributes.style "position" "fixed"
            , E.htmlAttribute <| Html.Attributes.style "top" "calc(90vh + 10px)"
            ]
            [ viewRefreshButton
            , viewDownloadButton
            ]
        ]


viewDownloadButton : Element Msg
viewDownloadButton =
    iconButton
        { onPress =
            Just <| DownloadModel
        , icon =
            FeatherIcons.download
        }


viewRefreshButton : Element Msg
viewRefreshButton =
    iconButton
        { onPress =
            Just <| GenerateModel
        , icon =
            FeatherIcons.refreshCw
        }


viewSizeRangeSelector : Model -> Element Msg
viewSizeRangeSelector model =
    E.column []
        [ slider
            { onChange = \newMin -> ChangedSizeRange ( newMin, Tuple.second model.sizeRange )
            , text = "min size"
            , min = 1
            , max = 1000
            , step = Just 1
            , value = Tuple.first model.sizeRange
            }
        , slider
            { onChange = \newMax -> ChangedSizeRange ( Tuple.first model.sizeRange, newMax )
            , text = "max size"
            , min = 1
            , max = 1000
            , step = Just 1
            , value = Tuple.second model.sizeRange
            }
        ]


viewGradientSelector : Model -> Element Msg
viewGradientSelector model =
    E.column
        []
    <|
        ([ NoGradient
         , UseGradient
            { innerOffset = 10
            , outerOffset = 100
            , outerAlphaScale = -0.5
            }
         ]
            |> List.map
                (\gradient ->
                    button
                        { onPress =
                            Just <| ChangedGradient gradient
                        , text =
                            gradientToString gradient
                        , selected =
                            gradientToString gradient == gradientToString model.gradient
                        }
                )
        )
            ++ [ case model.gradient of
                    NoGradient ->
                        E.none

                    UseGradient ({ innerOffset, outerOffset, outerAlphaScale } as gradient) ->
                        E.column []
                            [ slider
                                { onChange = \newOffset -> ChangedGradient <| UseGradient { gradient | innerOffset = round newOffset }
                                , text = "Inner Offset"
                                , min = 0
                                , max = 100
                                , step = Just 1
                                , value = toFloat innerOffset
                                }
                            , slider
                                { onChange = \newOffset -> ChangedGradient <| UseGradient { gradient | outerOffset = round newOffset }
                                , text = "Outer Offset"
                                , min = 0
                                , max = 100
                                , step = Just 1
                                , value = toFloat outerOffset
                                }
                            , slider
                                { onChange = \newScale -> ChangedGradient <| UseGradient { gradient | outerAlphaScale = newScale }
                                , text = "Outer Alpha Scale"
                                , min = -1
                                , max = 1
                                , step = Just 0.1
                                , value = outerAlphaScale
                                }
                            ]
               ]


gradientToString : Gradient -> String
gradientToString gradient =
    case gradient of
        NoGradient ->
            "No Gradient"

        UseGradient _ ->
            "Use Gradient"


viewBackgroundSelector : Model -> Element Msg
viewBackgroundSelector model =
    E.html
        (ColorPicker.view model.background model.backgroundColorPicker
            |> Html.map ChangedBackgroundColorPicker
        )


viewSkewSelector : Model -> Element Msg
viewSkewSelector model =
    E.column []
        [ slider
            { onChange = \newMin -> ChangedSkew ( newMin, Tuple.second model.skew )
            , text = "min skew"
            , min = 0.1
            , max = 2
            , step = Just 0.1
            , value = Tuple.first model.skew
            }
        , slider
            { onChange = \newMax -> ChangedSkew ( Tuple.first model.skew, newMax )
            , text = "max skew"
            , min = 0.1
            , max = 2
            , step = Just 0.1
            , value = Tuple.second model.skew
            }
        ]


viewSparsitySelector : Model -> Element Msg
viewSparsitySelector model =
    slider
        { onChange = ChangeSparsity
        , text = "sparsity"
        , min = 0.1
        , max = 1
        , step = Just 0.1
        , value = model.sparsity
        }


viewStepSelector : Model -> Element Msg
viewStepSelector model =
    slider
        { onChange = ChangeStep
        , text = "step"
        , min = 1
        , max = 100
        , step = Just 1
        , value = model.step
        }


viewHeightSelector : Model -> Element Msg
viewHeightSelector model =
    slider
        { onChange = ChangedHeight
        , text = "Height"
        , min = 100
        , max = 1500
        , step = Just 1
        , value = model.height
        }


viewWidthSelector : Model -> Element Msg
viewWidthSelector model =
    slider
        { onChange = ChangedWidth
        , text = "Width"
        , min = 100
        , max = 1500
        , step = Just 1
        , value = model.width
        }


viewStyleSelector : Model -> Element Msg
viewStyleSelector model =
    E.column []
        ([ DefaultStyle
         , PastelStyle
         , SoftStyle
         , HardStyle
         , LightStyle
         , PaleStyle
         ]
            |> List.map
                (\style ->
                    button
                        { onPress =
                            Just <| ChangedStyle style
                        , text =
                            styleToString style
                        , selected =
                            style == model.style
                        }
                )
        )


viewSchemeSelector : Model -> Element Msg
viewSchemeSelector model =
    E.column
        []
    <|
        ([ MonoScheme
         , ContrastScheme
         , TriadeScheme { distance = 1 }
         , TetradeScheme { distance = 1 }
         , AnalogicScheme
            { distance = 1
            , complemented = False
            }
         ]
            |> List.map
                (\scheme ->
                    button
                        { onPress =
                            Just <| ChangedScheme scheme
                        , text =
                            schemeToString scheme
                        , selected =
                            schemeToString scheme == schemeToString model.scheme
                        }
                )
        )
            ++ [ case model.scheme of
                    TriadeScheme { distance } ->
                        slider
                            { onChange = \newDistance -> ChangedScheme <| TriadeScheme { distance = newDistance }
                            , text = "Distance"
                            , min = 0
                            , max = 1
                            , step = Just 0.1
                            , value = distance
                            }

                    TetradeScheme { distance } ->
                        slider
                            { onChange = \newDistance -> ChangedScheme <| TetradeScheme { distance = newDistance }
                            , text = "Distance"
                            , min = 0
                            , max = 1
                            , step = Just 0.1
                            , value = distance
                            }

                    AnalogicScheme { distance, complemented } ->
                        E.column []
                            [ slider
                                { onChange =
                                    \newDistance ->
                                        ChangedScheme <|
                                            AnalogicScheme
                                                { distance = newDistance
                                                , complemented = complemented
                                                }
                                , text = "Distance"
                                , min = 0
                                , max = 1
                                , step = Just 0.1
                                , value = distance
                                }
                            , Input.checkbox []
                                { onChange =
                                    \newComplemented ->
                                        ChangedScheme <|
                                            AnalogicScheme
                                                { distance = distance
                                                , complemented = newComplemented
                                                }
                                , icon = Input.defaultCheckbox
                                , checked = complemented
                                , label =
                                    Input.labelRight []
                                        (E.text "complemented")
                                }
                            ]

                    _ ->
                        E.none
               ]


colors =
    { grey =
        E.rgb255 211 211 211
    , lightGrey =
        E.rgb255 233 233 233
    , white =
        E.rgb255 255 255 255
    }


h1 text =
    E.el
        [ Font.size 22
        , Font.bold
        , E.paddingEach { left = 0, right = 0, top = 15, bottom = 5 }
        ]
        (E.text text)


h2 text =
    E.el
        [ Font.size 18
        , Font.bold
        , E.paddingEach { left = 0, right = 0, top = 15, bottom = 5 }
        ]
        (E.text text)


h3 text =
    E.el
        [ Font.size 16
        , Font.bold
        , E.paddingEach { left = 0, right = 0, top = 10, bottom = 5 }
        ]
        (E.text text)


button { onPress, text, selected } =
    generalButton
        { onPress =
            onPress
        , label =
            E.el [] <|
                E.text text
        , selected =
            selected
        }


iconButton { onPress, icon } =
    generalButton
        { onPress =
            onPress
        , label =
            E.html (icon |> FeatherIcons.toHtml [])
        , selected =
            False
        }


generalButton { onPress, label, selected } =
    Input.button
        [ if selected then
            Background.color colors.grey

          else
            Background.color colors.white
        , E.mouseOver
            [ Background.color colors.lightGrey
            ]
        , E.paddingXY 10 5
        , Border.rounded 10
        ]
        { onPress =
            onPress
        , label =
            label
        }


slider { onChange, text, min, max, step, value } =
    Input.slider
        [ E.height (E.px 30)
        , E.behindContent
            (E.el
                [ E.width E.fill
                , E.height (E.px 2)
                , E.centerY
                , Background.color colors.grey
                , Border.rounded 2
                ]
                E.none
            )
        ]
        { onChange = onChange
        , label =
            Input.labelAbove []
                (h3 <| text ++ ": " ++ Round.round 1 value)
        , min = min
        , max = max
        , step = step
        , value = value
        , thumb =
            Input.defaultThumb
        }
