module Components exposing (..)

import Html exposing (Html, Attribute, text, div, button, section, h1, img, p, a, nav, span, i, table, thead, th, tr, td, h3, small, strong, br, ul, li, header, footer, input, label, select, option)
import Html.Attributes exposing (src, class, attribute, colspan, href, target, placeholder, type_, defaultValue, value)
import Html.Events exposing (onClick, onInput)


disabledAttribute : Bool -> Attribute msg
disabledAttribute isDisabled =
    if isDisabled then
        attribute "disabled" "true"
    else
        attribute "data-empty" ""


selectInput : Bool -> List ( String, String ) -> String -> String -> String -> (String -> msg) -> Html msg
selectInput isLoading optionsType fieldLabel fieldValue fieldIcon fieldMsg =
    let
        options =
            optionsType
                |> List.map
                    (\( optVal, optText ) ->
                        let
                            selectedAttr =
                                if optVal == fieldValue then
                                    [ value optVal
                                    , attribute "selected" ""
                                    ]
                                else
                                    [ value optVal ]
                        in
                            option
                                selectedAttr
                                [ text optText ]
                     -- option [ value optVal ] [ text optText ]
                    )

        loadingClass =
            if isLoading then
                " is-loading"
            else
                ""
    in
        div [ class "field" ]
            [ label [ class "label is-small" ]
                [ text fieldLabel ]
            , div [ class ("control has-icons-left" ++ loadingClass) ]
                [ div [ class "select is-small is-fullwidth" ]
                    [ select [ onInput fieldMsg, disabledAttribute isLoading ] options ]
                , icon fieldIcon False True
                ]
            ]


icon : String -> Bool -> Bool -> Html msg
icon icon spin isLeft =
    let
        spinner =
            if spin then
                " fa-spin"
            else
                ""

        className =
            "fa" ++ spinner ++ " fa-" ++ icon

        classIcon =
            if isLeft then
                "icon is-left"
            else
                "icon"
    in
        span [ class classIcon ]
            [ i [ class className ]
                []
            ]


loadingIcon : Html msg
loadingIcon =
    icon "circle-o-notch" True False


titleMenu : String -> List (Html msg) -> Html msg
titleMenu title menu =
    div [ class "level" ]
        [ div [ class "level-left" ]
            [ div [ class "level-item" ] [ h1 [ class "title" ] [ text title ] ] ]
        , div [ class "level-right" ]
            (menu
                |> List.map (\item -> div [ class "level-item" ] [ item ])
            )
        ]


columns : Bool -> List (Html msg) -> Html msg
columns isMultiline cols =
    let
        mlClass =
            if isMultiline then
                " is-multiline"
            else
                ""
    in
        div [ class ("columns" ++ mlClass) ]
            (cols
                |> List.map (\item -> div [ class "column" ] [ item ])
            )


modalCard : Int -> String -> msg -> List (Html msg) -> Maybe ( String, msg ) -> Maybe ( String, msg ) -> Html msg
modalCard isLoading title close body ok cancel =
    let
        loadingClass =
            if isLoading > 0 then
                " is-loading"
            else
                ""

        okButton =
            case ok of
                Just ( txt, msg ) ->
                    button
                        [ class ("button is-success" ++ loadingClass)
                        , onClick msg
                        , disabledAttribute (isLoading > 0)
                        ]
                        [ text txt ]

                Nothing ->
                    text ""

        cancelButton =
            case cancel of
                Just ( txt, msg ) ->
                    button
                        [ class ("button is-light" ++ loadingClass)
                        , onClick msg
                        , disabledAttribute (isLoading > 0)
                        ]
                        [ text txt ]

                Nothing ->
                    text ""
    in
        div [ class "modal is-active" ]
            [ div [ class "modal-background" ] []
            , div [ class "modal-card" ]
                [ header [ class "modal-card-head" ]
                    [ p [ class "modal-card-title" ]
                        [ text title ]
                    , button
                        [ class "delete"
                        , attribute "aria-label" "close"
                        , onClick close
                        ]
                        []
                    ]
                , section [ class "modal-card-body" ]
                    body
                , footer [ class "modal-card-foot" ]
                    [ okButton
                    , cancelButton
                    ]
                ]
            ]


basicFieldInput : Int -> String -> String -> String -> String -> (String -> msg) -> Bool -> String -> Html msg
basicFieldInput isLoading fieldLabel fieldValue fieldPlaceHolder fieldIcon fieldMsg readOnly fieldType =
    let
        loadingClass =
            if isLoading > 0 then
                " is-loading"
            else
                ""

        field =
            if readOnly then
                div [ class "field-read" ] [ text fieldValue ]
            else
                div
                    [ class
                        ("control has-icons-left has-icons-right"
                            ++ loadingClass
                        )
                    ]
                    [ input
                        [ class "input"
                        , placeholder fieldPlaceHolder
                        , type_ fieldType
                        , defaultValue fieldValue
                        , onInput fieldMsg
                        , disabledAttribute (isLoading > 0)
                        ]
                        []
                    , icon fieldIcon False True
                    ]
    in
        div [ class "field" ]
            [ label [ class "label" ]
                [ text fieldLabel ]
            , field
            ]


fieldInput : Int -> String -> String -> String -> String -> (String -> msg) -> Bool -> Html msg
fieldInput isLoading fieldLabel fieldValue fieldPlaceHolder fieldIcon fieldMsg readOnly =
    basicFieldInput isLoading fieldLabel fieldValue fieldPlaceHolder fieldIcon fieldMsg readOnly "text"


passwordInput : Int -> String -> String -> String -> String -> (String -> msg) -> Bool -> Html msg
passwordInput isLoading fieldLabel fieldValue fieldPlaceHolder fieldIcon fieldMsg readOnly =
    basicFieldInput isLoading fieldLabel fieldValue fieldPlaceHolder fieldIcon fieldMsg readOnly "password"
