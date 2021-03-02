port module Main exposing (..)

import Browser
import Element exposing (Attribute, Color, Element, alignRight, alignTop, centerY, column, el, fill, height, minimum, padding, paddingEach, px, rgb255, rgba, row, spacing, text, toRgb, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Hex
import Html exposing (Html)
import Html.Attributes exposing (attribute, style)
import Html.Events
import Json.Decode exposing (..)
import Json.Encode as Encode


type alias Model =
    { designName : String
    , layout : Layout

    -- This shouldn't be a list of strings, it should be a `Path`?
    , focus : Maybe (List String)
    , dragPath : Maybe BeaconType
    , dragStart : { x : Float, y : Float }
    }


type Atom
    = Text TextStyle String
    | Rectangle Int Int
    | Frame FrameData


type Msg
    = Focused (Maybe (List String))
    | NodeReplaced (List String) Layout
    | PathDeleted (List String)
    | SpacingUpdated (List String) Int
    | StrokeUpdated (List String) StrokeInfo
    | MsgPair ( Msg, Msg )
    | ReceivedDragEvent (Result Json.Decode.Error DragEvent)
    | FillUpdated (List String) Color
    | PaddingUpdated (List String) Padding


type BeaconType
    = OutlineItem (List String)
    | SpacingSlider (List String)
    | PaddingSlider (List String) -- TODO: this should include a direction (top/bottom/left/right/all)
    | StrokeWidthSlider (List String) StrokeInfo


type alias TextStyle =
    { size : Int
    , color : Color
    }



{-
   # FULLY EDITABLE
   ...

   # IN VIEW
   color
   size

   # IN MODEL
   ...

   # TODO
   wordWrapping (puts the text in a paragraph, let's you set line spacing with "spacing")

   Font Type:
   - typeface : String
   - serif
   - sansSerif
   - monospace


   Alignment and Spacing:
   - alignLeft
   - alignRight
   - center
   - justify
   - letterSpacing : Float
   - wordSpacing : Float

   Decoration:
   - underline
   - strike
   - italic
   - unitalicized : This will reset bold and italic.

   Font Weight
   - heavy
   - extraBold
   - bold
   - semiBold
   - medium
   - regular
   - light
   - extraLight
   - hairline

   Variants
   - smallCaps: Small caps are rendered using uppercase glyphs, but at the size of lowercase glyphs.
   - slashedZero: Add a slash when rendering 0
   - ligatures
   - ordinal: Oridinal markers like 1st and 2nd will receive special glyphs.
   - tabularNumbers: Number figures will each take up the same space, allowing them to be easily aligned, such as in tables.
   - stackedFractions : Render fractions with the numerator stacked on top of the denominator.
   - diagonalFractions : Render fractions
   - swash: Int [https://en.wikipedia.org/wiki/Swash_(typography)]
   - feature: Four-letter feature names from OpenType [https://docs.microsoft.com/en-us/typography/opentype/spec/featurelist]
   - indexed: A font variant might have multiple versions within the font. In these cases we need to specify the index of the version we want.

   shadow :
       { offset : ( Float, Float )
       , blur : Float
       , color : Color
       }
-}


type alias StrokeInfo =
    { width : Int
    , color : Color
    }


type alias FrameData =
    { direction : FrameDirection
    , children : List Layout
    , stroke : StrokeInfo
    , spacing : Int
    , fillColor : Color
    , padding : Padding
    }


type FrameDirection
    = HorizontalDirection
    | VerticalDirection


type alias Padding =
    { left : Int
    , right : Int
    , top : Int
    , bottom : Int
    }


type alias Layout =
    { name : String
    , value : Atom
    }


type alias Coord =
    { x : Float, y : Float }


type alias BeaconData =
    { id : BeaconType
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias DragEvent =
    { eventType : String
    , cursor : Coord
    , beacons : List BeaconData
    }


decodeDragEvent : Decoder DragEvent
decodeDragEvent =
    map3 DragEvent
        (at [ "eventType" ] string)
        (at [ "cursor" ]
            (map2 Coord
                (at [ "x" ] float)
                (at [ "y" ] float)
            )
        )
        (at [ "beacons" ]
            (list
                (map5 BeaconData
                    (at [ "id" ] decodeBeaconType)
                    (at [ "x" ] float)
                    (at [ "y" ] float)
                    (at [ "width" ] float)
                    (at [ "height" ] float)
                )
            )
        )


encodeBeaconType : BeaconType -> Encode.Value
encodeBeaconType beacon =
    case beacon of
        OutlineItem p ->
            Encode.object
                [ ( "beaconType", Encode.string "OutlineItem" )
                , ( "path", Encode.list Encode.string p )
                ]

        SpacingSlider p ->
            Encode.object
                [ ( "beaconType", Encode.string "SpacingSlider" )
                , ( "path", Encode.list Encode.string p )
                ]

        PaddingSlider p ->
            Encode.object
                [ ( "beaconType", Encode.string "PaddingSlider" )
                , ( "path", Encode.list Encode.string p )
                ]

        StrokeWidthSlider p strokeInfo ->
            Encode.object
                [ ( "beaconType", Encode.string "StrokeWidthSlider" )
                , ( "path", Encode.list Encode.string p )
                , ( "strokeInfo", encodeStrokeInfo strokeInfo )
                ]


decodeBeaconType : Decoder BeaconType
decodeBeaconType =
    field "beaconType" string
        |> andThen
            (\variantName ->
                case variantName of
                    "OutlineItem" ->
                        map OutlineItem
                            (field "path" (list string))

                    "SpacingSlider" ->
                        map SpacingSlider
                            (field "path" (list string))

                    "PaddingSlider" ->
                        map PaddingSlider
                            (field "path" (list string))

                    "StrokeWidthSlider" ->
                        map2 StrokeWidthSlider
                            (field "path" (list string))
                            (field "strokeInfo" decodeStrokeInfo)

                    _ ->
                        fail <| "Unknown beaconType: " ++ variantName
            )


encodeStrokeInfo : StrokeInfo -> Encode.Value
encodeStrokeInfo stroke =
    Encode.object
        [ ( "width", Encode.int stroke.width )
        , ( "color", encodeColor stroke.color )
        ]


decodeStrokeInfo : Decoder StrokeInfo
decodeStrokeInfo =
    map2 StrokeInfo
        (field "width" int)
        (field "color" decodeColor)


encodeColor : Color -> Encode.Value
encodeColor c =
    let
        channels =
            toRgb c
    in
    Encode.object
        [ ( "r", Encode.float channels.red )
        , ( "g", Encode.float channels.green )
        , ( "b", Encode.float channels.blue )
        , ( "a", Encode.float channels.alpha )
        ]


decodeColor : Decoder Color
decodeColor =
    map4 rgba
        (field "r" float)
        (field "g" float)
        (field "b" float)
        (field "a" float)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port dragEventReceiver : (Json.Decode.Value -> msg) -> Sub msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { designName = "test design"
      , layout =
            { name = "Frame 0"
            , value =
                Frame
                    { defaultVerticalData
                        | children =
                            [ { name = "header", value = Text defaultTextStyle "header" }
                            , { name = "menu"
                              , value =
                                    Frame
                                        { defaultHorizontalData
                                            | children =
                                                [ { name = "File", value = Text defaultTextStyle "File" }
                                                , { name = "Edit", value = Text defaultTextStyle "Edit" }
                                                , { name = "View", value = Text defaultTextStyle "View" }
                                                ]
                                            , spacing = 20
                                        }
                              }
                            , { name = "body"
                              , value =
                                    Frame
                                        { defaultHorizontalData
                                            | children =
                                                [ { name = "left pane", value = Text defaultTextStyle "Left Pane Text" }
                                                , { name = "right pane", value = Text defaultTextStyle "Right Pane Text" }
                                                , { name = "rect", value = Rectangle 400 600 }
                                                ]
                                        }
                              }
                            ]
                    }
            }
      , focus = Nothing
      , dragPath = Nothing
      , dragStart = { x = 0, y = 0 }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


nodeWithSpacing : Int -> Layout -> Layout
nodeWithSpacing spacing node =
    case node.value of
        Frame v ->
            { node | value = Frame { v | spacing = spacing } }

        _ ->
            node


nodeWithStroke : StrokeInfo -> Layout -> Layout
nodeWithStroke stroke node =
    case node.value of
        Frame v ->
            { node | value = Frame { v | stroke = stroke } }

        _ ->
            node


nodeWithFill : Color -> Layout -> Layout
nodeWithFill color node =
    case node.value of
        Frame v ->
            { node | value = Frame { v | fillColor = color } }

        _ ->
            node


nodeWithPadding : Padding -> Layout -> Layout
nodeWithPadding padding node =
    case node.value of
        Frame v ->
            { node | value = Frame { v | padding = padding } }

        _ ->
            node


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        Focused path ->
            { model | focus = path }

        NodeReplaced path newValue ->
            { model
                | layout =
                    replaceNode (tryTail path) newValue model.layout
            }

        PathDeleted path ->
            { model
                | layout =
                    deleteNode (tryTail path) model.layout
                , focus =
                    if model.focus == Just path then
                        Nothing

                    else
                        model.focus
            }

        SpacingUpdated path value ->
            { model
                | layout =
                    getNode (tryTail path) model.layout
                        |> Maybe.map (\oldNode -> replaceNode (tryTail path) (nodeWithSpacing value oldNode) model.layout)
                        |> Maybe.withDefault model.layout
            }

        StrokeUpdated path value ->
            { model
                | layout =
                    getNode (tryTail path) model.layout
                        |> Maybe.map (\oldNode -> replaceNode (tryTail path) (nodeWithStroke value oldNode) model.layout)
                        |> Maybe.withDefault model.layout
            }

        FillUpdated path value ->
            { model
                | layout =
                    getNode (tryTail path) model.layout
                        |> Maybe.map (\oldNode -> replaceNode (tryTail path) (nodeWithFill value oldNode) model.layout)
                        |> Maybe.withDefault model.layout
            }

        PaddingUpdated path value ->
            { model
                | layout =
                    getNode (tryTail path) model.layout
                        |> Maybe.map (\oldNode -> replaceNode (tryTail path) (nodeWithPadding value oldNode) model.layout)
                        |> Maybe.withDefault model.layout
            }

        ReceivedDragEvent (Ok e) ->
            case ( e.eventType, model.dragPath ) of
                ( "start", _ ) ->
                    { model
                        | dragPath = closestRect e.beacons e.cursor
                        , dragStart = e.cursor
                    }

                ( "move", Just (SpacingSlider dragPath) ) ->
                    updateModel (SpacingUpdated dragPath (round (model.dragStart.y - e.cursor.y))) model

                ( "move", Just (PaddingSlider dragPath) ) ->
                    updateModel
                        (PaddingUpdated dragPath
                            (let
                                newPadding =
                                    max (round (model.dragStart.y - e.cursor.y)) 0
                             in
                             { left = newPadding
                             , right = newPadding
                             , top = newPadding
                             , bottom = newPadding
                             }
                            )
                        )
                        model

                ( "move", Just (StrokeWidthSlider dragPath startingStroke) ) ->
                    updateModel
                        (StrokeUpdated dragPath
                            (strokeUpdateWidth (max (round (model.dragStart.y - e.cursor.y + toFloat startingStroke.width)) 0) startingStroke)
                        )
                        model

                ( "move", _ ) ->
                    model

                ( "stop", Just (SpacingSlider dragPath) ) ->
                    { model | dragPath = Nothing }

                ( "stop", Just (PaddingSlider dragPath) ) ->
                    { model | dragPath = Nothing }

                ( "stop", Just (StrokeWidthSlider dragPath _) ) ->
                    { model | dragPath = Nothing }

                ( "stop", Just (OutlineItem dragPath) ) ->
                    let
                        maybeDestinationPath =
                            closestRect
                                (List.filter
                                    (\b ->
                                        case b.id of
                                            OutlineItem _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    e.beacons
                                )
                                e.cursor
                    in
                    case maybeDestinationPath of
                        Just (OutlineItem destinationPath) ->
                            { model
                                | dragPath = Nothing
                                , layout =
                                    case
                                        ( getNode (tryTail dragPath) model.layout
                                        , getNode (tryTail destinationPath)
                                            -- We need to delete from the destination node in case the node being dragged is already a descendent of the destination
                                            (deleteNode (tryTail dragPath) model.layout)
                                        )
                                    of
                                        ( Just nodeToMove, Just newParent ) ->
                                            case newParent.value of
                                                Frame v ->
                                                    deleteNode (tryTail dragPath) model.layout
                                                        |> replaceNode (tryTail destinationPath)
                                                            { name = newParent.name
                                                            , value = Frame { v | children = nodeToMove :: v.children }
                                                            }

                                                _ ->
                                                    Debug.log "can't use as parent" model.layout

                                        _ ->
                                            -- You get this case when trying to reparent a node to its descendent
                                            Debug.log "could not find source or parent" model.layout
                            }

                        _ ->
                            Debug.log "No destination" model

                _ ->
                    { model | dragPath = Nothing }

        ReceivedDragEvent (Err e) ->
            Debug.log ("Failed to decode drag info" ++ Debug.toString e) model

        MsgPair ( first, second ) ->
            updateModel first model |> updateModel second


closestRect : List BeaconData -> { x : Float, y : Float } -> Maybe BeaconType
closestRect rects coord =
    rects
        |> List.filter (\r -> r.x <= coord.x && coord.x <= r.x + r.width)
        |> List.filter (\r -> r.y <= coord.y && coord.y <= r.y + r.height)
        |> List.head
        |> Maybe.map .id


tryTail : List a -> List a
tryTail lst =
    case lst of
        [] ->
            []

        v :: rest ->
            rest


getNode : List String -> Layout -> Maybe Layout
getNode path layout =
    case ( path, layout.value ) of
        ( [], _ ) ->
            Just layout

        ( nextPath :: rest, Rectangle _ _ ) ->
            -- Rectangle has no children, and were at the end of the line, so we can only return Nothing
            Nothing

        ( nextPath :: rest, Text _ _ ) ->
            -- Text has no children, and were at the end of the line, so we can only return Nothing
            Nothing

        ( nextPath :: rest, Frame v ) ->
            let
                matches =
                    v.children
                        |> List.indexedMap (\idx child -> ( idx, child ))
                        |> List.filter (\( idx, child ) -> child.name == nextPath)
            in
            case List.head matches of
                Nothing ->
                    -- No match!
                    Nothing

                Just ( idx, firstMatch ) ->
                    getNode rest firstMatch


deleteNode : List String -> Layout -> Layout
deleteNode path layout =
    case ( path, layout.value ) of
        ( [], _ ) ->
            Debug.todo "Can't delete empty path" layout

        ( nextPath :: rest, Rectangle _ _ ) ->
            Debug.todo ("Can't get children of rect, path: " ++ Debug.toString path) layout

        ( nextPath :: rest, Text _ _ ) ->
            Debug.todo ("Can't get children of text, path: " ++ Debug.toString path) layout

        ( nextPath :: [], Frame v ) ->
            { layout
                | value = Frame { v | children = v.children |> List.filter (\child -> child.name /= nextPath) }
            }

        ( nextPath :: rest, Frame v ) ->
            let
                matches =
                    v.children
                        |> List.indexedMap (\idx child -> ( idx, child ))
                        |> List.filter (\( idx, child ) -> child.name == nextPath)
            in
            case List.head matches of
                Nothing ->
                    Debug.todo ("No match for child of vertical" ++ Debug.toString ( path, v.children )) layout

                Just ( idx, oldValue ) ->
                    { layout
                        | value = Frame { v | children = listReplaceIndex (deleteNode rest oldValue) idx v.children }
                    }


replaceNode : List String -> Layout -> Layout -> Layout
replaceNode path newValue layout =
    case ( path, layout.value ) of
        ( [], _ ) ->
            newValue

        ( nextPath :: rest, Rectangle _ _ ) ->
            Debug.todo ("Can't get children of rect, path: " ++ Debug.toString path) layout

        ( nextPath :: rest, Text _ _ ) ->
            Debug.todo ("Can't get children of text, path: " ++ Debug.toString path) layout

        ( nextPath :: rest, Frame v ) ->
            let
                matches =
                    v.children
                        |> List.indexedMap (\idx child -> ( idx, child ))
                        |> List.filter (\( idx, child ) -> child.name == nextPath)
            in
            case List.head matches of
                Nothing ->
                    Debug.todo ("No match for child of vertical" ++ Debug.toString ( path, v.children )) layout

                Just ( idx, oldValue ) ->
                    { layout
                        | value = Frame { v | children = listReplaceIndex (replaceNode rest newValue oldValue) idx v.children }
                    }


listReplaceIndex : a -> Int -> List a -> List a
listReplaceIndex newValue replaceIdx lst =
    lst
        |> List.indexedMap
            (\idx value ->
                if idx == replaceIdx then
                    newValue

                else
                    value
            )


view : Model -> Html Msg
view model =
    Element.layout []
        (Element.column [ spacing 40, width fill, padding 4 ]
            [ Element.row [ alignTop, width fill ]
                [ viewOutline [] model model.layout
                , viewInspector model
                ]
            , viewAtom (popPath model.focus model.layout.name) model.layout.value
            ]
        )


viewInspector : Model -> Element Msg
viewInspector model =
    Element.el [ height (px 600), Border.width 1, width fill ]
        (case model.focus of
            Nothing ->
                text "Nothing Focused"

            Just path ->
                Element.column [ Font.size 32, spacing 14, padding 4 ]
                    ([ text
                        ("Focus: "
                            ++ String.join " → " path
                        )
                     , Input.text []
                        { onChange =
                            \newText ->
                                MsgPair
                                    ( NodeReplaced path
                                        { name = newText
                                        , value =
                                            (getNode (tryTail path) model.layout
                                                |> Maybe.withDefault
                                                    { name = "Something Broke"
                                                    , value = Text defaultTextStyle "SOmething broke"
                                                    }
                                            ).value
                                        }
                                    , Focused (Just (replaceLast newText path))
                                    )
                        , text = path |> listLast |> Maybe.withDefault "bad path"
                        , placeholder = Nothing
                        , label = Input.labelHidden ""
                        }
                     , Input.button
                        buttonStyle
                        { onPress = Just (PathDeleted path)
                        , label = text "Delete"
                        }
                     , Input.button
                        buttonStyle
                        { onPress =
                            getNode (tryTail path) model.layout
                                |> Maybe.map
                                    (\old ->
                                        NodeReplaced path
                                            { name = "hor", value = Frame { defaultHorizontalData | children = [ old ] } }
                                    )
                        , label = text "Wrap in row"
                        }
                     , Input.button
                        buttonStyle
                        { onPress =
                            getNode (tryTail path) model.layout
                                |> Maybe.map
                                    (\old ->
                                        NodeReplaced path
                                            { name = "col", value = Frame { defaultVerticalData | children = [ old ] } }
                                    )
                        , label = text "Wrap in column"
                        }
                     ]
                        ++ (getNode (tryTail path) model.layout
                                |> Maybe.map (selectionSpecificOptions path)
                                |> Maybe.withDefault []
                           )
                    )
        )


defaultStroke =
    { width = 1, color = black }


black =
    rgb255 0 0 0


white =
    rgb255 255 255 255


green =
    rgb255 0 100 0


strokeUpdateWidth : Int -> StrokeInfo -> StrokeInfo
strokeUpdateWidth newWidth stroke =
    { stroke | width = newWidth }


strokeInspector : List String -> StrokeInfo -> List (Element Msg)
strokeInspector path stroke =
    [ Element.row
        [ draggable (StrokeWidthSlider path stroke) ]
        [ text "StrokeWidth: "
        , text (String.fromInt stroke.width)
        ]
    , Element.row [ spacing 10 ]
        [ text "Stroke Color:"
        , Element.el
            [ Element.htmlAttribute (style "outline" "1px solid #000000")
            , Element.htmlAttribute (style "outline-offset" "-1px")
            , Background.color stroke.color
            , height fill
            , width (px 50)
            ]
            Element.none
        , text "(Pick:"
        , Element.html
            (Html.input
                [ attribute "type" "color"
                , attribute "value" (colorToHex stroke.color)
                , Html.Events.onInput (\hexstr -> StrokeUpdated path { stroke | color = colorFromHex hexstr })
                ]
                []
            )
        , text ")"
        ]
    ]


fillInspector : List String -> Color -> List (Element Msg)
fillInspector path color =
    [ Element.row [ spacing 10 ]
        [ text "Fill Color:"
        , Element.el
            [ Element.htmlAttribute (style "outline" "1px solid #000000")
            , Element.htmlAttribute (style "outline-offset" "-1px")
            , Background.color color
            , height fill
            , width (px 50)
            ]
            Element.none
        , text "(Pick:"
        , Element.html
            (Html.input
                [ attribute "type" "color"
                , attribute "value" (colorToHex color)
                , Html.Events.onInput (\hexstr -> FillUpdated path (colorFromHex hexstr))
                ]
                []
            )
        , text ")"
        ]
    ]


colorToHex : Color -> String
colorToHex c =
    let
        channels =
            toRgb c
    in
    "#"
        ++ zeroPad2 (Hex.toString (round (channels.red * 255)))
        ++ zeroPad2 (Hex.toString (round (channels.green * 255)))
        ++ zeroPad2 (Hex.toString (round (channels.blue * 255)))


zeroPad2 : String -> String
zeroPad2 s =
    case String.length s of
        0 ->
            "00"

        1 ->
            "0" ++ s

        _ ->
            s


colorFromHex : String -> Color
colorFromHex s =
    rgb255
        (Hex.fromString (String.slice 1 3 s) |> Result.withDefault 0)
        (Hex.fromString (String.slice 3 5 s) |> Result.withDefault 0)
        (Hex.fromString (String.slice 5 7 s) |> Result.withDefault 0)


spacingInspector : List String -> Int -> List (Element Msg)
spacingInspector path spacing =
    [ Element.row
        [ draggable (SpacingSlider path) ]
        [ text "Spacing: "
        , text (String.fromInt spacing)
        ]
    ]


paddingInspector : List String -> Padding -> List (Element Msg)
paddingInspector path padding =
    [ Element.row
        [ draggable (PaddingSlider path) ]
        [ text "Padding: "
        , text (String.fromInt padding.left)
        ]
    ]


selectionSpecificOptions : List String -> Layout -> List (Element Msg)
selectionSpecificOptions path layout =
    case layout.value of
        Text style t ->
            [ Input.text []
                { onChange =
                    \newText ->
                        NodeReplaced path
                            { name = path |> listLast |> Maybe.withDefault "bad path", value = Text style newText }
                , text = t
                , placeholder = Nothing
                , label = Input.labelHidden ""
                }
            ]

        Frame v ->
            [ Input.button
                buttonStyle
                { onPress =
                    Just
                        (NodeReplaced path
                            { name = path |> listLast |> Maybe.withDefault "bad path"
                            , value =
                                Frame
                                    { v
                                        | children =
                                            { name = nextName "New Text" (List.map .name v.children)
                                            , value = Text defaultTextStyle ""
                                            }
                                                :: v.children
                                    }
                            }
                        )
                , label = text "Add text element"
                }
            ]
                ++ strokeInspector path v.stroke
                ++ fillInspector path v.fillColor
                ++ spacingInspector path v.spacing
                ++ paddingInspector path v.padding

        _ ->
            []


defaultSpacing =
    0


defaultPadding =
    { left = 8
    , right = 8
    , top = 8
    , bottom = 8
    }


defaultTextStyle : TextStyle
defaultTextStyle =
    { size = 20
    , color = black
    }


defaultVerticalData =
    { direction = VerticalDirection, children = [], stroke = defaultStroke, spacing = defaultSpacing, fillColor = white, padding = defaultPadding }


defaultHorizontalData =
    { direction = HorizontalDirection, children = [], stroke = defaultStroke, spacing = defaultSpacing, fillColor = white, padding = defaultPadding }


nextName : String -> List String -> String
nextName name allNames =
    if List.member name allNames then
        nextNameInner name 0 allNames

    else
        name


nextNameInner : String -> Int -> List String -> String
nextNameInner namePrefix idx allNames =
    let
        newName =
            namePrefix ++ " " ++ String.fromInt idx
    in
    if List.member newName allNames then
        nextNameInner namePrefix (idx + 1) allNames

    else
        newName


replaceLast : a -> List a -> List a
replaceLast value lst =
    case lst of
        [] ->
            []

        last :: [] ->
            [ value ]

        head :: rest ->
            head :: replaceLast value rest


listLast : List a -> Maybe a
listLast lst =
    case lst of
        [] ->
            Nothing

        last :: [] ->
            Just last

        head :: rest ->
            listLast rest


buttonStyle =
    [ Border.width 1, Border.rounded 8, Background.color (rgb255 140 140 140), padding 4 ]



{--
Canvas Interactions:
- Canvas Panning
- Canvas Zooming
- Change Focus
- Context Menu?
--}


popPath : Maybe (List String) -> String -> Maybe (List String)
popPath path value =
    case path of
        Just (head :: rest) ->
            if head == value then
                Just rest

            else
                Nothing

        Just [] ->
            Nothing

        Nothing ->
            Nothing


viewAtom : Maybe (List String) -> Atom -> Element Msg
viewAtom path atom =
    let
        element =
            case atom of
                Text style t ->
                    el
                        [ Font.size style.size
                        , Font.color style.color
                        ]
                        (text t)

                Frame v ->
                    let
                        frameType =
                            if v.direction == VerticalDirection then
                                column

                            else
                                row
                    in
                    frameType
                        [ Element.htmlAttribute (style "outline" (String.fromInt v.stroke.width ++ "px solid #000000"))
                        , Element.htmlAttribute (style "outline-offset" ("-" ++ String.fromInt v.stroke.width ++ "px"))

                        --   Border.width v.stroke.width
                        --, Border.color v.stroke.color
                        , spacing v.spacing
                        , paddingEach v.padding
                        , Background.color v.fillColor
                        ]
                        (v.children
                            |> List.map (\layout -> viewAtom (popPath path layout.name) layout.value)
                        )

                Rectangle x y ->
                    Element.el
                        [ width (px x), height (px y) ]
                        Element.none
    in
    if path == Just [] then
        Element.el
            [ Element.htmlAttribute (style "outline" "2px dashed #18a0fb")
            ]
            element

    else
        element



{--
Outline:
- Change Focus
- Reparent (drag-and-drop)
- Context Menu?
--}


draggable : BeaconType -> Attribute Msg
draggable beacon =
    Element.htmlAttribute (attribute "data-beacon" (Encode.encode 0 (encodeBeaconType beacon)))


listStartsWith : List comparable -> List comparable -> Bool
listStartsWith prefix lst =
    case ( prefix, lst ) of
        ( [], _ ) ->
            True

        ( prefixHead :: prefixTail, lstHead :: lstTail ) ->
            if prefixHead == lstHead then
                listStartsWith prefixTail lstTail

            else
                False

        _ ->
            False


viewOutline : List String -> Model -> Layout -> Element Msg
viewOutline path model layout =
    Element.column
        [ alignTop
        , width fill
        ]
        [ Input.button
            ([ Element.focused []
             , draggable (OutlineItem (path ++ [ layout.name ]))
             ]
                ++ (case model.dragPath of
                        Just (OutlineItem s) ->
                            if listStartsWith s (path ++ [ layout.name ]) then
                                [ Font.color (rgb255 128 128 128) ]

                            else
                                [ Element.mouseOver [ Font.color (rgb255 200 0 0) ]
                                ]

                        _ ->
                            if model.focus == Just (path ++ [ layout.name ]) then
                                [ Font.color (rgb255 0 0 200) ]

                            else
                                [ Element.mouseOver [ Font.color (rgb255 200 0 0) ]
                                ]
                   )
            )
            { onPress =
                Just
                    (Focused
                        (Just (path ++ [ layout.name ]))
                    )
            , label = text layout.name
            }
        , Element.column [ paddingEach { left = 15, right = 0, top = 0, bottom = 0 } ]
            (case layout.value of
                Text _ _ ->
                    []

                Rectangle x y ->
                    []

                Frame v ->
                    v.children |> List.map (viewOutline (path ++ [ layout.name ]) model)
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    dragEventReceiver (\v -> ReceivedDragEvent (decodeValue decodeDragEvent v))
