module Journally exposing (..)

-- import Element exposing (..)
-- import Element.Background as Background
-- import Element.Border as Border
-- import Element.Events exposing (..)
-- import Element.Font as Font
-- import Element.Input as Input
--import Html.Styled exposing (..)
--import Html exposing (Attribute, Html, br, button, div, h1, h2, header, input, text, textarea)
--import Html.Attributes exposing (..)
--import Css.Global exposing (..)
--import Html.Events exposing (onClick, onInput)
--import Html.Attributes exposing (cols, placeholder, rows, value)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (cols, css, href, placeholder, rows, src, value)
import Html.Styled.Events exposing (onClick, onInput)
import Task
import Time


-- main : Html Msg
-- main =
--     layout [] <|
--         row [ Element.height fill, Element.width fill ]
--             [ channelPanel
--             , chatPanel
--             , Input.button
--                 [ Background.color (Element.rgb255 238 238 238)
--                 , Element.focused
--                     [ Background.color (Element.rgb255 238 0 238) ]
--                 ]
--                 { onPress = Just ClickMsg
--                 , label = Element.text "My Button"
--                 }
--             ]
-- channelPanel : Element msg
-- channelPanel =
--     column
--         [ Element.height fill
--         , Element.width <| fillPortion 1
--         , Background.color <| rgb255 92 99 118
--         , Font.color <| rgb255 255 255 255
--         ]
--         [ Element.text "channels" ]
--
--
-- chatPanel : Element msg
-- chatPanel =
--     column [ Element.height fill, Element.width <| fillPortion 5 ]
--         [ Element.text "chat" ]
--main =
--    Browser.sandbox { init = init, update = update, view = view }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias JournalEntry =
    { timeZone : Time.Zone
    , time : Time.Posix
    , content : String
    }


type alias Model =
    { entries : List JournalEntry
    , activeEntry : Maybe JournalEntry
    , currentTimeZone : Time.Zone
    }


type Msg
    = DoNothing
    | AddEntry
    | AddNewEntry Time.Posix
    | Change String
    | SaveEntry
    | AdjustTimeZone Time.Zone


init : () -> ( Model, Cmd Msg )
init _ =
    --init : Model
    --init =
    ( Model [] Nothing Time.utc, Task.perform AdjustTimeZone Time.here )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | currentTimeZone = zone }, Cmd.none )

        AddEntry ->
            ( model, Task.perform AddNewEntry Time.now )

        AddNewEntry time ->
            ( { model | activeEntry = Just (JournalEntry model.currentTimeZone time "") }, Cmd.none )

        SaveEntry ->
            case model.activeEntry of
                Nothing ->
                    ( model, Cmd.none )

                Just anEntry ->
                    ( { model
                        | entries = anEntry :: model.entries
                        , activeEntry = Nothing
                      }
                    , Cmd.none
                    )

        Change newContent ->
            case model.activeEntry of
                Nothing ->
                    ( model, Cmd.none )

                Just anEntry ->
                    ( { model | activeEntry = Just (JournalEntry anEntry.timeZone anEntry.time newContent) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.Styled.div []
        [ Html.Styled.h1 [ css [ textAlign center ] ]
            [ text "Journally" ]
        , viewEntries model
        , viewActiveEntry model
        , Html.Styled.br [] []
        , Html.Styled.div [ css [ textAlign center ] ]
            [ Html.Styled.button [ onClick AddEntry ] [ text "+" ]
            ]
        ]


viewActiveEntry : Model -> Html Msg
viewActiveEntry model =
    case model.activeEntry of
        Nothing ->
            Html.Styled.div [] []

        Just anEntry ->
            Html.Styled.div [ css [ textAlign center ] ]
                [ Html.Styled.div [] [ text (toDateTimeString anEntry.timeZone anEntry.time) ]
                , Html.Styled.textarea [ cols 80, rows 10, placeholder "Enter entry here", value anEntry.content, onInput Change ] []
                , Html.Styled.div [] [ button [ onClick SaveEntry ] [ text "Save" ] ]
                ]


viewEntries : Model -> Html Msg
viewEntries model =
    let
        entryDivs =
            List.map createEntry model.entries
    in
    Html.Styled.div [] entryDivs


createEntry entry =
    let
        hour =
            toHourString entry.timeZone entry.time

        minute =
            toMinuteString entry.timeZone entry.time

        second =
            toSecondString entry.timeZone entry.time
    in
    Html.Styled.div [ css [ textAlign center ] ]
        [ --[ div [] [ text entry.timeZone ]
          --, div [] [ text entry.time ]
          Html.Styled.div [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
        , Html.Styled.div [] [ text entry.content ]
        ]


toDateTimeString timeZone time =
    let
        hhmmss =
            toHHMMSS timeZone time

        day =
            toDayOfMonthString timeZone time

        month =
            toMonthString timeZone time

        year =
            toYearString timeZone time
    in
    day ++ " " ++ month ++ " " ++ year ++ " " ++ hhmmss


toDayOfMonthString timeZone time =
    let
        day =
            Time.toDay timeZone time
    in
    String.fromInt day


toMonthString timeZone time =
    let
        month =
            Time.toMonth timeZone time
    in
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


toYearString timeZone time =
    let
        year =
            Time.toYear timeZone time
    in
    String.fromInt year


toHHMMSS timeZone time =
    let
        hour =
            toHourString timeZone time

        minute =
            toMinuteString timeZone time

        second =
            toSecondString timeZone time
    in
    hour ++ ":" ++ minute ++ ":" ++ second


toHourString timeZone time =
    let
        value =
            Time.toHour timeZone time
    in
    if value < 10 then
        "0" ++ String.fromInt value
    else
        String.fromInt value


toMinuteString timeZone time =
    let
        value =
            Time.toMinute timeZone time
    in
    if value < 10 then
        "0" ++ String.fromInt value
    else
        String.fromInt value


toSecondString timeZone time =
    let
        value =
            Time.toSecond timeZone time
    in
    if value < 10 then
        "0" ++ String.fromInt value
    else
        String.fromInt value