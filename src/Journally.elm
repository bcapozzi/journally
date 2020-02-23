port module Journally exposing (..)

import Browser
import Browser.Dom
import Debug
import Html exposing (Html, br, button, div, h1, text, textarea)
import Html.Attributes exposing (class, cols, href, id, placeholder, rows, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, bool, field, int, map2, map3, string)
import Json.Encode as Encode
import Task
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    load Load


type alias JournalEntry =
    { timestamp : Int
    , content : String
    , isEditable : Bool
    }


type alias Model =
    { entries : List JournalEntry
    , activeEntry : Maybe JournalEntry
    , currentTimeZone : Time.Zone
    , activeTimestamp : Maybe Int
    }


type Msg
    = DoNothing
    | AddEntry
    | AddNewEntry Time.Posix
    | Change String
    | SaveEntry
    | AdjustTimeZone Time.Zone
    | DoLoad
    | Load String
    | Edit Int


port save : String -> Cmd msg


port load : (String -> msg) -> Sub msg


port doload : () -> Cmd msg


init : () -> ( Model, Cmd Msg )
init _ =
    --init : Model
    --init =
    ( Model [] Nothing Time.utc Nothing, Task.perform AdjustTimeZone Time.here )


toSingleString : String -> List String -> String
toSingleString stringSoFar remainingTokens =
    case remainingTokens of
        [] ->
            stringSoFar

        first :: rest ->
            if String.length stringSoFar > 0 then
                toSingleString (stringSoFar ++ ";" ++ first) rest
            else
                toSingleString (stringSoFar ++ first) rest


entryJson : JournalEntry -> Encode.Value
entryJson entry =
    Encode.object
        [ ( "content", Encode.string entry.content )
        , ( "timestamp", Encode.int entry.timestamp )
        , ( "isEditable", Encode.bool entry.isEditable )
        ]


encodeAsJson : List JournalEntry -> Encode.Value
encodeAsJson entries =
    Encode.list entryJson entries


toEntriesJson : List JournalEntry -> String
toEntriesJson entries =
    Encode.encode 0 (encodeAsJson entries)


toEntryJson : JournalEntry -> String
toEntryJson entry =
    Encode.encode 0 (entryJson entry)



--toEntryString : JournalEntry -> String
--toEntryString entry =
--    let
--        millis_string =
--            String.fromInt (Time.posixToMillis entry.time)
--    in
--    "JOURNALLY_CONTENT:" ++ entry.content ++ ":JOURNALLY_TIMESTAMP:" ++ millis_string


toString : List JournalEntry -> String
toString entries =
    toEntriesJson entries



--    let
--        entry_strings =
--            List.map (\x -> toEntryJson x) entries
--    in
--    toSingleString "" entry_strings
--        millis =
--            List.map (\x -> Time.posixToMillis x.time) entries
--
--        as_strings =
--            List.map (\x -> String.fromInt x) millis
--    in
--    toSingleString "" as_strings


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | currentTimeZone = zone }, Cmd.none )

        AddEntry ->
            ( model, Task.perform AddNewEntry Time.now )

        AddNewEntry time ->
            let
                timestamp =
                    Time.posixToMillis time

                x =
                    Debug.log ("Timestamp" ++ String.fromInt timestamp)

                entryToAdd =
                    JournalEntry (Time.posixToMillis time) "" True
            in
            ( { model
                | activeTimestamp = Just timestamp
                , activeEntry = Just entryToAdd
              }
            , focusActiveEntry
            )

        SaveEntry ->
            case model.activeEntry of
                Nothing ->
                    ( model, Cmd.none )

                Just anEntry ->
                    let
                        entryToSave =
                            JournalEntry anEntry.timestamp anEntry.content False
                    in
                    ( { model
                        | entries = entryToSave :: model.entries
                        , activeEntry = Nothing
                        , activeTimestamp = Nothing
                      }
                    , save (toString (entryToSave :: model.entries))
                    )

        Change newContent ->
            case model.activeEntry of
                Nothing ->
                    ( model, Cmd.none )

                Just anEntry ->
                    ( { model | activeEntry = Just (JournalEntry anEntry.timestamp newContent anEntry.isEditable) }, Cmd.none )

        DoLoad ->
            ( model, doload () )

        Load value ->
            let
                entries =
                    decodeFromJson value

                x =
                    Debug.log "LOAD VALUE" value
            in
            ( { model | entries = entries }, Cmd.none )

        Edit timestamp ->
            let
                entry =
                    List.filter (\x -> x.timestamp == timestamp) model.entries
            in
            case entry of
                [] ->
                    ( model, Cmd.none )

                first :: rest ->
                    ( { model
                        | activeEntry = Just (JournalEntry first.timestamp first.content first.isEditable)
                        , entries = removeEntry timestamp model.entries
                      }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


removeEntry : Int -> List JournalEntry -> List JournalEntry
removeEntry timestamp entries =
    List.filter (\x -> not (x.timestamp == timestamp)) entries


findEntryWithTimestamp : List JournalEntry -> Int -> Maybe JournalEntry
findEntryWithTimestamp entries timestampToFind =
    let
        results =
            List.filter (\x -> x.timestamp == timestampToFind) entries
    in
    case results of
        [] ->
            Nothing

        first :: rest ->
            Just first


listOfEntriesDecoder : Decode.Decoder (List JournalEntry)
listOfEntriesDecoder =
    Decode.list entryDecoder


entryDecoder : Decode.Decoder JournalEntry
entryDecoder =
    map3 JournalEntry
        (field "timestamp" int)
        (field "content" string)
        (field "isEditable" bool)


decodeFromJson : String -> List JournalEntry
decodeFromJson value =
    case Decode.decodeString listOfEntriesDecoder value of
        Ok decoded ->
            decoded

        Err message ->
            []


view : Model -> Browser.Document Msg
view model =
    { title = "Journally"
    , body =
        [ div []
            [ h1 [ class "title" ]
                [ text "Journally" ]
            , div [ class "centered" ] [ button [ onClick DoLoad ] [ text "Load" ] ]
            , br [] []
            , div [ class "centered" ]
                [ button [ onClick AddEntry ] [ text "+" ]
                ]
            , br [] []
            , viewActiveEntry model
            , viewEntries model
            ]
        ]
    }


viewActiveEntry : Model -> Html Msg
viewActiveEntry model =
    case model.activeEntry of
        Nothing ->
            div [] []

        Just anEntry ->
            div [ class "centered" ]
                [ div [] [ text (toDateTimeString model.currentTimeZone (Time.millisToPosix anEntry.timestamp)) ]
                , textarea [ cols 80, rows 10, placeholder "Enter entry here", value anEntry.content, onInput Change, id "active-entry-field" ] []
                , div [] [ button [ onClick SaveEntry ] [ text "Save" ] ]
                ]


focusActiveEntry : Cmd Msg
focusActiveEntry =
    Task.attempt (\_ -> DoNothing) (Browser.Dom.focus "active-entry-field")


viewEntries : Model -> Html Msg
viewEntries model =
    let
        entryDivs =
            List.map (\x -> createEntry model.activeTimestamp model.currentTimeZone x) model.entries
    in
    div [] entryDivs


createEntry : Maybe Int -> Time.Zone -> JournalEntry -> Html Msg
createEntry activeTimestamp timezone entry =
    let
        timeString =
            toDateTimeString timezone (Time.millisToPosix entry.timestamp)
    in
    case activeTimestamp of
        Nothing ->
            div [ class "centered", class "entry" ]
                [ div [ class "timestamp" ] [ text timeString ]
                , div [ class "content" ] [ text entry.content ]
                , button [ onClick (Edit entry.timestamp) ] [ text "Edit" ]
                ]

        Just aTimestamp ->
            div [ class "centered", class "entry" ]
                [ div [ class "timestamp" ] [ text timeString ]
                , div [ class "content" ] [ text entry.content ]
                , button [ onClick (Edit entry.timestamp) ] [ text "Edit" ]
                ]



--            div [ class "centered", class "entry" ]
--                [ div [ class "timestamp" ] [ text timeString ]
--                , div [ class "content" ] [ text entry.content ]
--                , button [ onClick (Edit entry.timestamp) ] [ text "Edit" ]
--                ]


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
