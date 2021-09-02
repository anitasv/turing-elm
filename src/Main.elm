module Main exposing (..)

import Array exposing (Array, fromList, get)
import Browser
import Browser.Dom exposing (Element)
import Dict exposing (Dict)
import Element
    exposing
        ( alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , none
        , padding
        , paddingXY
        , paragraph
        , px
        , rgb255
        , row
        , scrollbarY
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html, th)
import Html.Attributes exposing (align)
import Json.Decode as D
import Json.Encode as E
import Tuple exposing (first, second)


init : Model
init =
    { turing = "{}"
    , code = D.decodeString decoder "{}"
    , turingState = Nothing
    , state =
        { tape = fromList [] -- Will always be odd
        , index = After 0
        }
    }


type Index
    = Before Int
    | OnTape Int Int
    | After Int


type alias Tape =
    Array Int


type alias TuringState =
    { tape : Tape
    , index : Index
    }


type alias Model =
    { turing : String
    , code : Result D.Error TuringCode
    , turingState : Maybe String
    , state : TuringState
    }


type Msg
    = UpdateTuring String
    | RightShift
    | LeftShift
    | Flip
    | ApplyMove Action
    | Step


type alias TuringCode =
    { initialState : String
    , haltingStates : List String
    , states : Dict String Condition
    }


type alias Effect =
    { action : Action
    , next : String
    }


type alias Condition =
    { if0 : Effect
    , if1 : Effect
    }


actionDecoder : D.Decoder Action
actionDecoder =
    D.map2 Action
        (D.field "write" D.bool)
        (D.field "right" D.bool)


effectDecoder : D.Decoder Effect
effectDecoder =
    D.map2 Effect
        (D.field "action" actionDecoder)
        (D.field "next" D.string)


conditionDecoder : D.Decoder Condition
conditionDecoder =
    D.map2 Condition
        (D.field "if0" effectDecoder)
        (D.field "if1" effectDecoder)


decoder : D.Decoder TuringCode
decoder =
    D.map3 TuringCode
        (D.field "initialState" D.string)
        (D.field "haltingStates" (D.list D.string))
        (D.field "states" (D.dict conditionDecoder))


initState : Result D.Error TuringCode -> Maybe String
initState rtc =
    case rtc of
        Ok tc ->
            Just tc.initialState

        Err _ ->
            Nothing


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTuring str ->
            let
                code =
                    D.decodeString decoder str
            in
            { model
                | turing = str
                , code = code
                , turingState = initState code
            }

        RightShift ->
            { model
                | state =
                    { tape = model.state.tape
                    , index = shiftRight model.state.index model.state.tape
                    }
            }

        LeftShift ->
            { model
                | state =
                    { tape = model.state.tape
                    , index = shiftLeft model.state.index model.state.tape
                    }
            }

        ApplyMove action ->
            { model
                | state = apply action model.state
            }

        Flip ->
            { model
                | state = flip model.state
            }

        Step ->
            makeStep model


handleChange : String -> Msg
handleChange str =
    UpdateTuring str


square : Bool -> Element.Element Msg
square parity =
    Element.el
        [ Border.width 1
        , height (px 15)
        , width (px 15)
        , centerX
        , centerY
        , Background.color
            (if parity then
                rgb255 0 0 0

             else
                rgb255 255 255 255
            )
        ]
        none


pivot : Bool -> Element.Element Msg
pivot parity =
    row [ centerX ] [ text "->", square parity, text "<-" ]


type alias CellView =
    Element.Element Msg


type alias TapeView =
    List CellView


isEven : Int -> Bool
isEven n =
    modBy 2 n == 0


jumpToStart : Tape -> Index
jumpToStart tape =
    if Array.isEmpty tape then
        After 0

    else
        OnTape 0 0


jumpRight : Int -> Tape -> Index
jumpRight pos tape =
    if pos + 1 < Array.length tape then
        OnTape (pos + 1) 0

    else
        After 0


toCellsRight : Index -> Tape -> Int -> TapeView
toCellsRight index arr rows =
    case index of
        Before shift ->
            if rows <= shift then
                List.repeat rows (square False)

            else
                List.append
                    (List.repeat shift (square False))
                    (toCellsRight (jumpToStart arr) arr (rows - shift))

        OnTape pos shift ->
            let
                n =
                    Array.get pos arr
                        |> Maybe.withDefault 0
            in
            if rows <= (n - shift) then
                List.repeat rows (square (isEven pos))

            else
                List.append
                    (List.repeat (n - shift) (square (isEven pos)))
                    (toCellsRight (jumpRight pos arr) arr (rows - (n - shift)))

        After shift ->
            List.repeat rows (square False)


jumpLeft : Int -> Tape -> Index
jumpLeft pos arr =
    case get (pos - 1) arr of
        Nothing ->
            Before 1

        Just n ->
            OnTape (pos - 1) (n - 1)


jumpToEnd : Tape -> Index
jumpToEnd arr =
    let
        pos =
            Array.length arr - 1
    in
    case get pos arr of
        Nothing ->
            After 0

        Just n ->
            OnTape pos (n - 1)


toCellsLeft : Index -> Tape -> Int -> TapeView
toCellsLeft index arr rows =
    case index of
        Before shift ->
            List.repeat rows (square False)

        OnTape pos shift ->
            let
                take =
                    shift + 1

                mySquare =
                    square (isEven pos)
            in
            if rows <= take then
                List.repeat rows mySquare

            else
                List.append
                    (toCellsLeft (jumpLeft pos arr) arr (rows - take))
                    (List.repeat take mySquare)

        After shift ->
            let
                mySquare =
                    square False

                take =
                    shift + 1
            in
            if rows <= take then
                List.repeat rows mySquare

            else
                List.append
                    (toCellsLeft (jumpToEnd arr) arr (rows - take))
                    (List.repeat take mySquare)


shiftLeft : Index -> Array Int -> Index
shiftLeft index arr =
    case index of
        Before shift ->
            Before (shift + 1)

        OnTape pos shift ->
            if shift > 0 then
                OnTape pos (shift - 1)

            else
                jumpLeft pos arr

        After shift ->
            if shift > 0 then
                After (shift - 1)

            else
                jumpToEnd arr


shiftRight : Index -> Array Int -> Index
shiftRight index arr =
    case index of
        Before shift ->
            if shift > 0 then
                Before (shift - 1)

            else
                jumpToStart arr

        OnTape pos shift ->
            let
                n =
                    get pos arr |> Maybe.withDefault 0
            in
            if shift + 1 < n then
                OnTape pos (shift + 1)

            else
                jumpRight pos arr

        After shift ->
            if Array.length arr == 0 then
                After 0

            else
                After (shift + 1)


toCells : TuringState -> Int -> TapeView
toCells ts halfRows =
    List.concat
        [ toCellsLeft (shiftLeft ts.index ts.tape) ts.tape halfRows
        , [ pivot (read ts) ]
        , toCellsRight (shiftRight ts.index ts.tape) ts.tape halfRows
        ]


toText : TuringState -> String
toText ts =
    Debug.toString ts


toDebug : Result D.Error TuringCode -> String
toDebug tc =
    case tc of
        Err err ->
            Debug.toString err

        Ok c ->
            "Parse Success!"


styledButton : String -> Msg -> Element.Element Msg
styledButton str msg =
    Input.button
        [ padding 6
        , Border.width 1
        , Border.rounded 10
        , Background.color <| rgb255 0x21 0x85 0xD0
        , Font.color <| rgb255 0xFF 0xFF 0xFF
        , Element.mouseOver
            [ Background.color <| rgb255 0x16 0x78 0xC2 ]
        , Element.focused
            [ Background.color <| rgb255 0x0D 0x71 0xBB ]
        ]
        { onPress = Just msg
        , label = text str
        }


disabledButton : String -> Element.Element Msg
disabledButton str =
    Input.button
        [ padding 6
        , Border.width 1
        , Border.rounded 10
        , Background.color <| rgb255 192 193 194
        , Font.color <| rgb255 0x00 0x00 0x00
        ]
        { onPress = Nothing
        , label = text str
        }


view : Model -> Html Msg
view model =
    Element.layout [ height fill ]
        (row
            [ width (fillPortion 1), height fill ]
            [ column
                [ padding 10, spacing 7, width (fillPortion 1), height fill ]
                [ row
                    [ padding 10, spacing 7, width fill ]
                    [ styledButton "Left Shift" LeftShift
                    , styledButton "Right Shift" RightShift
                    , styledButton "Flip" Flip
                    , case model.turingState of
                        Nothing ->
                            disabledButton "Step"

                        Just _ ->
                            styledButton "Step" Step
                    ]
                , column [ width fill ]
                    [ paragraph [] [ text (toText model.state) ]
                    , paragraph [] [ text (toDebug model.code) ]
                    , paragraph [] [ text (Debug.toString model.turingState) ]
                    ]
                , row [ width fill, height fill, scrollbarY ]
                    [ Input.multiline [ height fill ]
                        { onChange = handleChange
                        , text = model.turing
                        , placeholder = Maybe.Nothing
                        , label =
                            Input.labelAbove []
                                (text "Turing machine")
                        , spellcheck = False
                        }
                    ]
                ]
            , column
                [ padding 10
                , spacing 7
                , width (fillPortion 1)
                , height fill
                ]
                (toCells model.state 6)
            ]
        )


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


at : Index -> Tape -> Bool
at index arr =
    case index of
        Before shift ->
            False

        OnTape pos shift ->
            isEven pos

        After shift ->
            False


read : TuringState -> Bool
read ts =
    at ts.index ts.tape


type alias Action =
    { write : Bool
    , right : Bool
    }


shiftParametric : Bool -> Index -> Tape -> Index
shiftParametric right index tape =
    if right then
        shiftRight index tape

    else
        shiftLeft index tape


{-| Till nth position (excluding)
-}
first : Int -> Tape -> Tape
first n arr =
    if n >= 0 then
        Array.slice 0 n arr

    else
        Array.empty


{-| From nth position (including) to end.
-}
last : Int -> Tape -> Tape
last n arr =
    if n >= 0 then
        Array.slice n (Array.length arr) arr

    else
        arr


{-| Parity preserving operation: parity(len1 + 1) = parity(len)
-}
merge : Tape -> Int -> Tape
merge tape term =
    let
        len =
            Array.length tape
    in
    case get (len - 1) tape of
        Nothing ->
            Array.empty

        Just n ->
            Array.set (len - 1) (n + term) tape


{-| Parity preserving operation: parity(len1 + len2) = parity(len)
-}
mergeArr : Tape -> Tape -> Tape
mergeArr tape1 tape2 =
    let
        len =
            Array.length tape1

        term =
            get 0 tape2
    in
    case get (len - 1) tape1 of
        Nothing ->
            tape2

        Just n ->
            case term of
                Nothing ->
                    tape1

                Just m ->
                    Array.append
                        (Array.set (len - 1) (n + m) tape1)
                        (last 1 tape2)


reduceArr : Array Int -> Int -> Int -> Int -> Array Int -> Tape -> Index -> ( Tape, Index )
reduceArr prefix left middle right suffix tape index =
    -- if left = 0 then prefix + middle
    -- if right = 0 then middle + suffix
    let
        newPrefix =
            if left == 0 then
                merge prefix middle

            else
                Array.append prefix (fromList [ left, middle ])

        newTapePre =
            if right == 0 then
                mergeArr newPrefix suffix

            else
                Array.append (Array.push right newPrefix) suffix

        ( newTape, newIndex ) =
            let
                tapeLen =
                    Array.length newTapePre

                lastEntry =
                    get (tapeLen - 1) newTapePre
            in
            if isEven tapeLen then
                case lastEntry of
                    Nothing ->
                        ( Array.empty, After 0 )

                    Just n ->
                        ( first (tapeLen - 1) newTapePre, After (n - 1) )

            else if Array.length newPrefix == 0 then
                ( newTapePre, Before 1 )

            else
                let
                    lastPos =
                        Array.length newPrefix - 1
                in
                case get lastPos newPrefix of
                    Nothing ->
                        ( newTapePre, Before 1 )

                    Just n ->
                        ( newTapePre, OnTape lastPos (n - 1) )
    in
    ( newTape, newIndex )


splitArr : Tape -> Index -> ( Tape, Index )
splitArr tape index =
    case index of
        Before shift ->
            if shift == 1 then
                case get 0 tape of
                    Nothing ->
                        ( fromList [ 1 ], OnTape 0 0 )

                    Just n ->
                        ( Array.set 0 (n + 1) tape, OnTape 0 0 )

            else
                ( Array.append (fromList [ 1, shift - 1 ]) tape, OnTape 0 0 )

        OnTape pos shift ->
            let
                prefix =
                    first pos tape

                suffix =
                    last (pos + 1) tape

                val =
                    get pos tape |> Maybe.withDefault 0
            in
            reduceArr prefix shift 1 (val - shift - 1) suffix tape index

        After shift ->
            if Array.length tape == 0 then
                ( fromList [ 1 ], OnTape 0 0 )

            else
                reduceArr tape shift 1 0 Array.empty tape index


flip : TuringState -> TuringState
flip ts =
    let
        ( newTape, newIndex ) =
            splitArr ts.tape ts.index
    in
    { tape = newTape
    , index = newIndex
    }


apply : Action -> TuringState -> TuringState
apply action ts =
    if action.write == read ts then
        { tape = ts.tape
        , index = shiftParametric action.right ts.index ts.tape
        }

    else
        let
            ( newTape, newIndex ) =
                splitArr ts.tape ts.index
        in
        { tape = newTape
        , index = shiftParametric action.right newIndex newTape
        }


makeEffect : Effect -> Model -> Model
makeEffect effect model =
    { model
        | state = apply effect.action model.state
        , turingState = Just effect.next
    }


makeStep : Model -> Model
makeStep model =
    case model.turingState of
        Nothing ->
            model

        Just str ->
            case model.code of
                Err _ ->
                    model

                Ok tc ->
                    let
                        maybeState =
                            Dict.get str tc.states
                    in
                    case maybeState of
                        Nothing ->
                            model

                        Just aState ->
                            let
                                effect =
                                    if read model.state then
                                        aState.if1

                                    else
                                        aState.if0
                            in
                            makeEffect effect model
