port module Main exposing (..)

import Browser
import Color exposing (Color)
import Color.Convert
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
import Json.Encode as Encode
import TypedSvg as Svg
import TypedSvg.Attributes as Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), px)


port generateModelPort : { scheme : Encode.Value, style : String, width : Float, height : Float } -> Cmd msg


port receivedModelPort : (Encode.Value -> msg) -> Sub msg


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { filters : List Filter
    , background : Color
    , scheme : Scheme
    , style : Style
    , width : Float
    , height : Float
    }


decodeModel : Decoder Model
decodeModel =
    Field.require "filters" (Decode.list decodeFilter) <|
        \filters ->
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
                                                    Decode.succeed
                                                        { filters = filters
                                                        , background = background
                                                        , scheme = scheme
                                                        , style = style
                                                        , width = width
                                                        , height = height
                                                        }


type alias Filter =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , color : Color
    }


decodeFilter : Decoder Filter
decodeFilter =
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


encodeStyle : Style -> String
encodeStyle style =
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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { filters = []
            , scheme = MonoScheme
            , style = DefaultStyle
            , background = Color.black
            , width = 600
            , height = 600
            }
    in
    ( model
    , generateModel model
    )


type Msg
    = ReceivedModel Encode.Value
    | ChangedScheme Scheme


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedModel modelValue ->
            receivedModel modelValue model

        ChangedScheme newScheme ->
            changedScheme newScheme model


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
        Decode.decodeValue decodeModel modelValue
    , Cmd.none
    )


generateModel : Model -> Cmd msg
generateModel model =
    generateModelPort
        { scheme =
            encodeScheme model.scheme
        , style =
            encodeStyle model.style
        , width =
            model.width
        , height =
            model.height
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    receivedModelPort ReceivedModel


view : Model -> Html Msg
view model =
    E.layout
        [ Font.size 16
        , E.width E.fill
        ]
    <|
        E.row [ E.width E.fill ]
            [ E.html <|
                Svg.svg
                    [ Attributes.width (px model.width)
                    , Attributes.height (px model.height)
                    , Attributes.viewBox 0 0 model.width model.height
                    ]
                <|
                    List.map viewFilter model.filters
            , viewControlPanel model
            ]


viewFilter : Filter -> Svg Msg
viewFilter { x, y, width, height, color } =
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
    -- , scheme : Scheme
    -- , style : Style
    -- , width : Float
    -- , height : Float
    -- }
    E.column
        [ E.alignTop
        , E.padding 10
        ]
        [ h1 "Controls"
        , h2 "Scheme"
        , E.column
            []
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
        , case model.scheme of
            TriadeScheme { distance } ->
                slider
                    { onChange = \newDistance -> ChangedScheme <| TriadeScheme { distance = newDistance }
                    , text = "distance"
                    , min = 0
                    , max = 1
                    , step = Nothing
                    , value = distance
                    }

            TetradeScheme { distance } ->
                slider
                    { onChange = \newDistance -> ChangedScheme <| TetradeScheme { distance = newDistance }
                    , text = "distance"
                    , min = 0
                    , max = 1
                    , step = Nothing
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
                        , text = "distance"
                        , min = 0
                        , max = 1
                        , step = Nothing
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
    , light =
        E.rgb255 255 255 255
    }


h1 text =
    E.el
        [ Font.size 22
        , Font.bold
        , E.paddingEach { left = 0, right = 0, top = 10, bottom = 10 }
        ]
        (E.text text)


h2 text =
    E.el
        [ Font.size 18
        , Font.bold
        , E.paddingEach { left = 0, right = 0, top = 5, bottom = 5 }
        ]
        (E.text text)


h3 text =
    E.el
        [ Font.size 16
        , Font.bold
        , E.paddingEach { left = 0, right = 0, top = 5, bottom = 5 }
        ]
        (E.text text)


button { onPress, text, selected } =
    Input.button
        [ if selected then
            Background.color colors.grey

          else
            Background.color colors.light
        , E.paddingXY 10 5
        , Border.rounded 10
        ]
        { onPress =
            onPress
        , label =
            E.el [] <|
                E.text text
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
                (h3 text)
        , min = min
        , max = max
        , step = step
        , value = value
        , thumb =
            Input.defaultThumb
        }
