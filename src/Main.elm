module Main exposing (Model, init, main, update)

import Browser
import Char exposing (isLower, toLower)
import Debug exposing (toString)
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input as Input
import GenericDict
import Html exposing (Html)
import List exposing (sort)
import List.Extra
import SearchBox


languages =
    sort
        [ "Awk"
        , "C"
        , "C++"
        , "C#"
        , "Elm"
        , "Go"
        , "Haskell"
        , "Python"
        , "JavaScript"
        , "K"
        , "Ruby"
        , "OpenBSD"
        , "Rust"
        , "Scala"
        , "Swift"
        ]



-- Should include Perl, Smalltalk, Ada, and Lisp


values =
    GenericDict.fromList toString
        [ ( "Approachability", [ "Awk", "JavaScript" ] )
        , ( "Availability", [ "JavaScript" ] )
        , ( "Compatibility", [] )
        , ( "Composability", [ "Rust", "Go", "Scala", "Haskell" ] )
        , ( "Debuggability", [] )
        , ( "Expressiveness", [ "Rust", "K", "Awk", "Haskell", "Ruby", "Scala" ] )
        , ( "Extensibility", [ "Rust", "C++", "Python" ] )
        , ( "Integrity", [ "Rust", "Scala" ] )
        , ( "Interoperability", [ "Rust", "C", "Scala", "Swift" ] )
        , ( "Maintainability", [ "Python" ] ) -- Readability?
        , ( "Measurability", [] )
        , ( "Operability", [] )
        , ( "Performance", [ "Rust", "C", "C++", "K", "Elm", "Go", "Swift" ] )
        , ( "Portability", [ "C", "C++", "C#", "Python" ] )
        , ( "Productivity", [ "C#", "Elm", "Go", "Ruby" ] )
        , ( "Resilience", [] )
        , ( "Rigor", [ "Rust" ] )
        , ( "Robustness", [ "Rust", "C#", "Scala" ] )
        , ( "Safety", [ "Rust", "Go" ] )
        , ( "Security", [ "Rust", "OpenBSD" ] )
        , ( "Simplicity", [ "C", "Awk", "Elm", "Go", "Python", "Ruby" ] )
        , ( "Stability", [] )
        , ( "Transparency", [] )
        , ( "Velocity", [ "Awk", "Python", "JavaScript", "Swift" ] ) -- Learnablity?
        ]



-- Values to consider:
-- , ( "Reliability", [] )
-- ( "Durability", ["C#"] )
-- , ( "Thoroughness", [] )


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { language : Maybe String
    , languageText : String
    , languageSearchBox : SearchBox.State
    }


init : Model
init =
    { language = Just "Rust"
    , languageText = ""
    , languageSearchBox = SearchBox.init
    }


type Msg
    = ChangedLanguageSearchBox (SearchBox.ChangeEvent String)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangedLanguageSearchBox changeEvent ->
            case changeEvent of
                SearchBox.SelectionChanged lang ->
                    { model | language = Just lang }

                SearchBox.TextChanged text ->
                    { model
                        | language = Nothing
                        , languageText = text
                        , languageSearchBox = SearchBox.reset model.languageSearchBox
                    }

                SearchBox.SearchBoxChanged subMsg ->
                    { model | languageSearchBox = SearchBox.update subMsg model.languageSearchBox }


viewFaded : Bool -> String -> Element.Element Msg
viewFaded selected text =
    let
        fade =
            if selected then
                1.0

            else
                0.3

        orange =
            if selected then
                rgb255 235 100 0

            else
                rgb255 0 0 0
    in
    Element.row
        [ Element.alpha fade
        , Element.width Element.fill
        , Element.Font.size 30
        , Element.padding 10
        ]
    <|
        [ Element.el [ Element.Font.color orange ] <| Element.text " • "
        , Element.text text
        ]


viewValue : Model -> String -> Element.Element Msg
viewValue model selectedValue =
    let
        langs =
            Maybe.withDefault [] <| GenericDict.get toString selectedValue values

        length =
            langs
                |> List.filter (\lang -> lang == Maybe.withDefault "" model.language)
                |> List.length

        selected =
            length > 0
    in
    viewFaded selected selectedValue


paddingPercent : Int -> Int -> Element.Element Msg -> Element.Element Msg
paddingPercent width height element =
    let
        xRemaining =
            (100 - width) // 2

        yRemaining =
            (100 - height) // 2
    in
    Element.row [ Element.width Element.fill, Element.height Element.fill ]
        [ Element.el [ Element.width <| Element.fillPortion xRemaining, Element.height Element.fill ] Element.none
        , Element.el [ Element.width <| Element.fillPortion width, Element.height Element.fill ] <|
            Element.column [ Element.width Element.fill, Element.height Element.fill ]
                [ Element.el [ Element.width Element.fill, Element.height <| Element.fillPortion yRemaining ] Element.none
                , Element.el [ Element.width Element.fill, Element.height <| Element.fillPortion height ] <|
                    element
                , Element.el [ Element.width Element.fill, Element.height <| Element.fillPortion yRemaining ] Element.none
                ]
        , Element.el [ Element.width <| Element.fillPortion xRemaining, Element.height Element.fill ] Element.none
        ]


paddingPercentWidth : Int -> Element.Element Msg -> Element.Element Msg
paddingPercentWidth width element =
    let
        xRemaining =
            (100 - width) // 2
    in
    Element.row [ Element.width Element.fill ]
        [ Element.el [ Element.width <| Element.fillPortion xRemaining, Element.height Element.fill ] Element.none
        , Element.el [ Element.width <| Element.fillPortion width, Element.height Element.fill ] <|
            element
        , Element.el [ Element.width <| Element.fillPortion xRemaining, Element.height Element.fill ] Element.none
        ]


viewValues : Model -> Element.Element Msg
viewValues model =
    values
        |> GenericDict.keys
        |> sort
        |> List.map (\v -> viewValue model v)
        -- This is the HEIGHT of the grid
        |> List.Extra.greedyGroupsOf 8
        |> List.map
            (\col ->
                Element.column
                    [ Element.height Element.fill
                    , Element.spaceEvenly
                    ]
                    col
            )
        |> Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spaceEvenly
            ]
        |> paddingPercent 50 85


viewHeader : Model -> Element.Element Msg
viewHeader model =
    let
        box =
            SearchBox.input []
                { onChange = ChangedLanguageSearchBox
                , text = model.languageText
                , selected = model.language
                , options = Just languages
                , label = Input.labelAbove [] Element.none
                , placeholder = Nothing
                , toLabel = \language -> language
                , filter = \query language -> String.contains (String.toLower query) (String.toLower language)
                , state = model.languageSearchBox
                }

        hr =
            Element.el
                [ Element.width Element.fill
                , Element.paddingXY 0 20

                -- , Element.paddingEach { bottom = 20, top = 20, left = 0, right = 0}
                ]
            <|
                Element.el
                    [ Element.width Element.fill
                    , Element.Border.color <| rgb255 240 95 22
                    , Element.Border.widthEach
                        { bottom = 1
                        , left = 0
                        , right = 0
                        , top = 0
                        }
                    ]
                    Element.none
    in
    Element.column [ Element.width Element.fill, centerX ]
        [ Element.el [ Element.Font.size 50, Element.paddingXY 0 35, Element.Font.color <| rgb255 240 95 22, Element.Font.bold, centerX ] <|
            Element.text "Language Core Values"
        , Element.row [ Element.width <| Element.maximum 500 Element.fill, centerX ]
            [ Element.text <| "Pick a language: ", box ]
        , hr
        ]
        |> paddingPercentWidth 70


viewFooter : Model -> Element.Element Msg
viewFooter model =
    Element.row
        [ Element.Font.color <| rgba255 0 0 0 0.3
        , Element.height <| Element.px 75
        , Element.width Element.fill
        , Element.spaceEvenly
        ]
        [ Element.column []
            [ Element.text "Created by Jacob Haig"
            , Element.text "Inspired by Bryan Cantrill's talk on Platform Values"
            ]
        , Element.column
            [ Element.alignRight ]
            [ Element.text "Something wrong or missing? Let me know!"
            , Element.row [ Element.alignRight ]
                [ Element.text "Create a pull request on "
                , Element.link [ Element.Font.color <| rgb255 34 155 220 ]
                    { url = "https://github.com/JacobHaig/Language-Core-Values"
                    , label = Element.text "GitHub"
                    }
                ]
            ]
        ]
        |> paddingPercentWidth 70


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column [ Element.width Element.fill, Element.height Element.fill ]
            [ viewHeader model
            , viewValues model
            , viewFooter model
            ]
