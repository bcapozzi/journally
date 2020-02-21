module Forms exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" ""


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []



-- TODO:  Add all the requirements:  length > X, contains a Capital, contains a Symbol, contains a Number


viewValidation : Model -> Html msg
viewValidation model =
    let
        div1 =
            validateContainsSymbol model.password

        div2 =
            validateContainsNumber model.password

        div3 =
            validateMinimumLength model.password

        div4 =
            validatePasswordsMatch model.password model.passwordAgain
    in
    div [] [ div1, div2, div3, div4 ]


validateContainsNumber : String -> Html msg
validateContainsNumber password =
    let
        ( isOK, context ) =
            containsNumber password
    in
    if isOK then
        div [ style "color" "green" ] [ text context ]
    else
        div [ style "color" "red" ] [ text context ]


containsNumber : String -> ( Bool, String )
containsNumber password =
    let
        numbers =
            [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

        temp1 =
            List.map (\x -> String.contains x password) numbers

        temp2 =
            List.filter (\x -> x) temp1

        --    results = List.map (\x -> String.contains \x password) symbols
        --    isTrue = List.filter (x -> x == True) result
        --  in
        --    case isTrue of
        --      [] -> (False, "Contains special symbol")
        --      first::rest -> (True, "Contains special symbol")
    in
    case temp2 of
        [] ->
            ( False, "Contains at least one digit (0-9)" )

        first :: rest ->
            ( True, "Contains at least one digit (0-9)+" )



--    if String.length model.password > 0 && model.password == model.passwordAgain then
--        div [ style "color" "green" ] [ text "OK" ]
--    else
--        div [ style "color" "red" ] [ text "Passwords do not match!" ]


validatePasswordsMatch : String -> String -> Html msg
validatePasswordsMatch password passwordAgain =
    let
        ( isOK, context ) =
            passwordsMatch password passwordAgain
    in
    if isOK then
        div [ style "color" "green" ] [ text context ]
    else
        div [ style "color" "red" ] [ text context ]


passwordsMatch : String -> String -> ( Bool, String )
passwordsMatch password passwordAgain =
    if String.length password > 0 && password == passwordAgain then
        ( True, "Passwords match" )
    else
        ( False, "Passwords match" )


validateMinimumLength : String -> Html msg
validateMinimumLength password =
    let
        ( isOK, context ) =
            isMinimumLength password
    in
    if isOK then
        div [ style "color" "green" ] [ text context ]
    else
        div [ style "color" "red" ] [ text context ]


isMinimumLength : String -> ( Bool, String )
isMinimumLength password =
    if String.length password >= 8 then
        ( True, "Is at least 8 characters" )
    else
        ( False, "Is at least 8 characters" )


validateContainsSymbol : String -> Html msg
validateContainsSymbol password =
    let
        ( isOK, context ) =
            containsSymbol password
    in
    if isOK then
        div [ style "color" "green" ] [ text context ]
    else
        div [ style "color" "red" ] [ text context ]


containsSymbol : String -> ( Bool, String )
containsSymbol password =
    let
        symbols =
            [ "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "-", "+" ]

        temp1 =
            List.map (\x -> String.contains x password) symbols

        temp2 =
            List.filter (\x -> x) temp1

        --    results = List.map (\x -> String.contains \x password) symbols
        --    isTrue = List.filter (x -> x == True) result
        --  in
        --    case isTrue of
        --      [] -> (False, "Contains special symbol")
        --      first::rest -> (True, "Contains special symbol")
    in
    case temp2 of
        [] ->
            ( False, "Contains at least one of !@#$%^&*()-+" )

        first :: rest ->
            ( True, "Contains at least one of !@#$%^&*()-+" )
