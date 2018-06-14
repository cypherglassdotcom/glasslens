module Main exposing (..)

import Html exposing (Html, text, div, section, h1, img, p, a, nav, span)
import Html.Attributes exposing (src, class, attribute)
import Html.Events exposing (onClick)


---- PROGRAM & INITS ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- MODEL ----


type Step
    = Welcome
    | ListBps
    | EnterPk
    | TransactionConfirmation
    | SuccessFinal


type alias Producer =
    { account : String
    , rank : Int
    , votes : Float
    , percentage : Float
    , url : String
    }


type alias Model =
    { producers : List Producer
    , step : Step
    , pk : Maybe String
    }


initialModel : Model
initialModel =
    { producers = []
    , step = Welcome
    , pk = Nothing
    }


apiUrl : String
apiUrl =
    "http://api.cypherglass.com:8888"



---- UPDATE ----


type Msg
    = StartVoting
    | ConfirmVote
    | GenerateTransaction
    | ConfirmTransaction
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartVoting ->
            ( { model | step = ListBps, pk = Nothing }, Cmd.none )

        ConfirmVote ->
            ( { model | step = EnterPk }, Cmd.none )

        GenerateTransaction ->
            ( { model | step = TransactionConfirmation }, Cmd.none )

        ConfirmTransaction ->
            ( { model | step = SuccessFinal }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.step of
        Welcome ->
            welcomeView model

        ListBps ->
            listBpsView model

        EnterPk ->
            enterPkView model

        TransactionConfirmation ->
            transactionView model

        SuccessFinal ->
            successView model


welcomeView : Model -> Html Msg
welcomeView model =
    div [ class "welcome" ]
        [ img [ src "assets/logo-cg.svg" ] []
        , p [] [ text "Welcome to Cypherglass Voting Tool!" ]
        , p [] [ text "This is a safe and easy tool to vote for EOS Block Producers" ]
        , a [ onClick StartVoting ] [ text "Start Voting Session" ]
        ]


pageView : Model -> List (Html msg) -> Html msg
pageView model content =
    div []
        [ topMenu model
        , section [ class "section" ] [ div [ class "container" ] content ]
        ]


topMenu : Model -> Html msg
topMenu model =
    nav
        [ attribute "aria-label" "main navigation"
        , class "navbar topcg"
        , attribute "role" "navigation"
        ]
        [ div [ class "navbar-brand logo" ]
            [ img [ class "logo-img", src "assets/logo_horizontal.svg" ] []
            , span [ class "title-span is-hidden-mobile" ] [ text "VOTING TOOL" ]
            , span [ class "title-span is-hidden-tablet" ] [ text "VT" ]
            ]
        , div [ class "navbar-menu" ]
            [ div [ class "navbar-end" ]
                [ text "" ]
            ]
        ]


listBpsView : Model -> Html Msg
listBpsView model =
    pageView model
        [ h1 [ class "title" ] [ text "Producers List" ]
        , p [] [ text "Please vote Wisely!" ]
        , a [ onClick ConfirmVote ] [ text "Confirm Vote" ]
        ]


enterPkView : Model -> Html Msg
enterPkView model =
    pageView model
        [ h1 [ class "title" ] [ text "Enter your Private Key" ]
        , p [] [ text "It's safer if you disconnect" ]
        , a [ onClick GenerateTransaction ] [ text "Confirm Voting Transaction" ]
        ]


transactionView : Model -> Html Msg
transactionView model =
    pageView model
        [ h1 [ class "title" ] [ text "Review Vote" ]
        , p [] [ text "Confirm your Vote Transaction data:" ]
        , p [] [ text "A, B, C" ]
        , a [ onClick ConfirmTransaction ] [ text "Submit Vote" ]
        ]


successView : Model -> Html Msg
successView model =
    pageView model
        [ h1 [ class "title" ] [ text "Success" ]
        , p [] [ text "Thanks for voting using Cypherglass Voting Tool!" ]
        , p [] [ text "Share voting in Twitter | Facebook" ]
        , a [ onClick StartVoting ] [ text "New Voting Session" ]
        ]
