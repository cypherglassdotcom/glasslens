module Main exposing (..)

import Html exposing (Html, text, div, button, section, h1, img, p, a, nav, span, i, table, thead, th, tr, td)
import Html.Attributes exposing (src, class, attribute, colspan, href, target)
import Html.Events exposing (onClick)
import Json.Encode as JE
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Http
import Time


---- PROGRAM & INITS ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second Tick
        ]



---- MODEL ----


type Step
    = Welcome
    | ListBps
    | EnterPk
    | TransactionConfirmation
    | SuccessFinal


type alias Producer =
    { account : String
    , totalVotes : Float
    , producerKey : String
    , isActive : Bool
    , url : String
    , rank : Int
    , votesPercentage : Float
    }


type NotificationType
    = Success String
    | Warning String
    | Error String


type alias Notification =
    { notification : NotificationType
    , time : Time.Time
    , id : String
    }


type alias Model =
    { producers : List Producer
    , step : Step
    , pk : Maybe String
    , isLoading : Int
    , notifications : List Notification
    , currentTime : Time.Time
    }


initialModel : Model
initialModel =
    { producers = []
    , step = Welcome
    , pk = Nothing
    , isLoading = 0
    , notifications = []
    , currentTime = 0
    }


producersApiUrl : String
producersApiUrl =
    "https://api.cypherglass.com/v1/chain/get_table_rows"



---- UPDATE ----


type Msg
    = StartVoting
    | ConfirmVote
    | GenerateTransaction
    | ConfirmTransaction
    | ProducersResponse (Result Http.Error (List Producer))
    | Tick Time.Time
    | DeleteNotification String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model
                | currentTime = time
                , notifications = updateNotifications model.notifications model.currentTime
              }
            , Cmd.none
            )

        StartVoting ->
            ( { model
                | step = ListBps
                , pk = Nothing
                , isLoading = model.isLoading + 1
              }
            , getProducersList
            )

        ConfirmVote ->
            ( { model | step = EnterPk }, Cmd.none )

        GenerateTransaction ->
            ( { model | step = TransactionConfirmation }, Cmd.none )

        ConfirmTransaction ->
            ( { model | step = SuccessFinal }, Cmd.none )

        ProducersResponse (Ok producers) ->
            ( { model | producers = producers, isLoading = model.isLoading - 1 }, Cmd.none )

        ProducersResponse (Err _) ->
            addError model "listProducersFail" "Fail to list Producers"

        DeleteNotification id ->
            let
                notifications =
                    model.notifications
                        |> List.filter (\notification -> notification.id /= id)
            in
                ( { model | notifications = notifications }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


addError : Model -> String -> String -> ( Model, Cmd msg )
addError model msgId msgBody =
    let
        notifications =
            Notification (Error msgBody) model.currentTime msgId
                :: model.notifications
    in
        ( { model
            | isLoading = model.isLoading - 1
            , notifications = notifications
          }
        , Cmd.none
        )


updateNotifications : List Notification -> Time.Time -> List Notification
updateNotifications notifications currentTime =
    notifications
        |> List.filter
            (\notification ->
                (currentTime - notification.time) < 10000
            )


isActiveNumToBool : JD.Decoder Bool
isActiveNumToBool =
    JD.int |> JD.andThen (\val -> JD.succeed (val == 1))


producersDecoder : JD.Decoder (List Producer)
producersDecoder =
    JD.at [ "rows" ] (JD.list producerDecoder)


producerDecoder : JD.Decoder Producer
producerDecoder =
    JDP.decode Producer
        |> JDP.required "owner" JD.string
        |> JDP.required "total_votes" JD.float
        |> JDP.required "producer_key" JD.string
        |> JDP.required "is_active" isActiveNumToBool
        |> JDP.required "url" JD.string
        |> JDP.hardcoded 5000
        |> JDP.hardcoded 0.0


getProducersList : Cmd Msg
getProducersList =
    let
        body =
            JE.object
                [ ( "scope", JE.string "eosio" )
                , ( "code", JE.string "eosio" )
                , ( "table", JE.string "producers" )
                , ( "json", JE.bool True )
                , ( "limit", JE.int 5000 )
                ]

        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = producersApiUrl
                , body = Http.jsonBody body
                , expect = Http.expectJson producersDecoder
                , timeout = Nothing
                , withCredentials = False
                }

        cmd =
            Http.send ProducersResponse request
    in
        cmd



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


notification : Notification -> Html Msg
notification notification =
    let
        ( txt, messageClass ) =
            case notification.notification of
                Success str ->
                    ( str, "is-success" )

                Warning str ->
                    ( str, "is-warning" )

                Error str ->
                    ( str, "is-danger" )
    in
        div [ class ("notification on " ++ messageClass) ]
            [ button
                [ class "delete"
                , onClick (DeleteNotification notification.id)
                ]
                []
            , text txt
            ]


notificationsToasts : Model -> Html Msg
notificationsToasts model =
    div [ class "toast" ] (model.notifications |> List.map notification)


pageView : Model -> List (Html Msg) -> Html Msg
pageView model content =
    div []
        [ topMenu model
        , notificationsToasts model
        , section [ class "section" ] [ div [ class "container" ] content ]
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


topMenu : Model -> Html msg
topMenu model =
    let
        isLoadingSpan =
            if model.isLoading > 0 then
                span [ class "loading-msg" ]
                    [ text "Please wait... "
                    , loadingIcon
                    ]
            else
                text ""
    in
        nav
            [ attribute "aria-label" "main navigation"
            , class "navbar topcg"
            , attribute "role" "navigation"
            ]
            [ div [ class "navbar-brand logo" ]
                [ img [ class "logo-img", src "assets/logo_horizontal.svg" ] []
                , span [ class "title-span is-hidden-mobile" ] [ text "VOTING TOOL" ]
                , span [ class "title-span is-hidden-tablet" ] [ text "VT" ]
                , isLoadingSpan
                ]
            , div [ class "navbar-menu" ]
                [ div [ class "navbar-end" ] [ text "" ]
                ]
            ]


producerRow : Producer -> Html msg
producerRow producer =
    tr []
        [ td [] [ icon "check" False False ]
        , td [] [ text producer.account ]
        , td [] [ a [ href producer.url, target "_blank" ] [ text producer.url ] ]
        , td [] [ text "353,879,888 EOS" ]
        ]


producersList : List Producer -> Html msg
producersList producers =
    let
        producersRows =
            if List.length producers > 0 then
                producers
                    |> List.sortBy .rank
                    |> List.map producerRow
            else
                [ tr []
                    [ td
                        [ colspan 4, class "has-text-centered" ]
                        [ text "Producers not loaded" ]
                    ]
                ]
    in
        table [ class "table is-striped is-hoverable is-fullwidth" ]
            [ thead []
                (tr []
                    [ th [] [ text "" ]
                    , th [] [ text "Producer" ]
                    , th [] [ text "Website" ]
                    , th [] [ text "Vote Stats" ]
                    ]
                    :: producersRows
                )
            ]


listBpsView : Model -> Html Msg
listBpsView model =
    pageView model
        [ h1 [ class "title" ] [ text "Producers List" ]
        , producersList model.producers
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
