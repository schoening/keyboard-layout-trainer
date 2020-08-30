module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Code
import Element exposing (Element, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as JD
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Maybe



---- MODEL ----


type alias Model =
    { code : Array (Array String)
    , rowIndex : Int
    , columnIndex : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { code = Code.toCode Code.code
      , rowIndex = 0
      , columnIndex = 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = HandleKeyboardEvent KeyboardEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKeyboardEvent event ->
            let
                _ =
                    Debug.log ""

                -- event
                maybeEventKey =
                    case event.key of
                        Just value ->
                            if value == "Enter" then
                                Just "\n"

                            else
                                Just value

                        Nothing ->
                            event.key

                ( ri, ci ) =
                    getNextPosition model.code model.rowIndex model.columnIndex

                _ =
                    Debug.log "( ri, ci )"

                -- ( ri, ci )
                maybeKey =
                    getColumnValueFromTable model.rowIndex model.columnIndex model.code

                _ =
                    Debug.log "maybeKey"

                -- maybeKey
            in
            case ( maybeEventKey, maybeKey ) of
                ( Just eventKey, Just key ) ->
                    let
                        _ =
                            Debug.log "( Just eventKey, Just key )"

                        -- ( eventKey, key )
                    in
                    if eventKey == key then
                        let
                            _ =
                                Debug.log "if eventKey == key then"

                            -- key
                        in
                        ( { model | rowIndex = ri, columnIndex = ci }, Cmd.none )

                    else
                        let
                            _ =
                                Debug.log "else" ( eventKey, key )
                        in
                        ( model, Cmd.none )

                _ ->
                    let
                        _ =
                            Debug.log "_ ->"
                    in
                    ( model, Cmd.none )


getNextPosition : Array (Array String) -> Int -> Int -> ( Int, Int )
getNextPosition table rowIndex columnIndex =
    let
        -- possible scenarios:
        -- current row -> next char
        -- end of row -> next row -> first char
        -- last row -> next char
        -- last row -> last char
        isLast row_ index =
            Array.length row_ - 1 == index

        row =
            Maybe.withDefault Array.empty <|
                Array.get rowIndex table

        _ =
            Debug.log "isLast table rowIndex"

        -- (isLast table rowIndex)
    in
    if isLast table rowIndex then
        if isLast row columnIndex then
            let
                _ =
                    Debug.log "position" "last last"
            in
            ( Array.length table - 1, Array.length row - 1 )

        else
            let
                _ =
                    Debug.log "position" "last _"
            in
            ( Array.length table - 1, columnIndex + 1 )

    else
        let
            _ =
                Debug.log "position"

            -- "_ last"
        in
        if isLast row columnIndex then
            let
                _ =
                    Debug.log "position" "_ last"
            in
            ( rowIndex + 1, 0 )

        else
            let
                _ =
                    Debug.log "position" "_ _"
            in
            ( rowIndex, columnIndex + 1 )


getColumnValueFromTable : Int -> Int -> Array (Array String) -> Maybe String
getColumnValueFromTable ri ci table =
    Array.get ri table
        |> Maybe.andThen (\mbRow -> Array.get ci mbRow)



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.family
            [ Font.typeface "Courier New"
            , Font.monospace
            ]
        ]
        (Code.toHtml model.code model.rowIndex model.columnIndex)


myRowOfStuff =
    row [ width fill, centerY, spacing 30 ]
        [ myElement
        , myElement
        , el [ alignRight ] myElement
        ]


myElement : Element msg
myElement =
    el
        [ Background.color (rgb255 240 0 245)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 30
        , Font.family
            [ Font.typeface "Courier New"
            , Font.monospace
            ]

        -- , htmlAttribute (style "tabindex" "0")
        ]
        (text "stylish!")



---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown (JD.map HandleKeyboardEvent decodeKeyboardEvent)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init =
            \_ ->
                let
                    _ =
                        Debug.log "code"

                    -- (Code.toCode Code.code)
                in
                init
        , update = update
        , subscriptions = subscriptions
        }
