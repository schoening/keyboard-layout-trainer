module Code exposing (..)

import Array exposing (Array)
import Element exposing (Element, column, el, fill, height, minimum, px, rgba, row, text, width)
import Element.Background as Background
import Element.Font as Font


toHtml : Array (Array String) -> Int -> Int -> Element msg
toHtml array2d highlightRow highlightColumn =
    let
        hR =
            highlightRow

        hC =
            highlightColumn

        _ =
            Debug.log "( hR, hC )"

        -- ( hR, hC )
    in
    column [] <|
        List.indexedMap
            (\ri list ->
                row [] <| List.indexedMap (\ci char -> character char ri ci hR hC) list
            )
            (toList2D array2d)


character : String -> Int -> Int -> Int -> Int -> Element msg
character char ri ci hR hC =
    let
        attributes =
            (if ri == hR && ci == hC then
                [ Background.color (rgba 0 255 0 1)
                ]

             else
                []
            )
                |> List.append
                    [ width (fill |> minimum 12)
                    , height (fill |> minimum 10)

                    -- , Background.color (rgba 0 0 0 0.5)
                    , Font.size 14
                    ]
    in
    el attributes (text char)


toCode : String -> Array (Array String)
toCode text =
    String.split "\n" text
        |> List.map (\row -> row ++ "\n")
        |> List.map
            (\row ->
                String.split "" row
                    |> (\column ->
                            if List.length column /= 1 then
                                List.reverse column
                                    |> List.drop 1
                                    |> List.reverse

                            else
                                column
                       )
            )
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> toArray2D


toList2D : Array (Array a) -> List (List a)
toList2D array2d =
    Array.map
        (\row -> Array.toList row)
        array2d
        |> Array.toList


toArray2D : List (List a) -> Array (Array a)
toArray2D list2d =
    List.map
        (\row -> Array.fromList row)
        list2d
        |> Array.fromList


code : String
code =
    """module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
"""
