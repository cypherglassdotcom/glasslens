port module Main exposing (..)

import Html exposing (Html, text, div, button, section, h1, img, p, a, nav, span, i, table, thead, th, tr, td)
import Html.Attributes exposing (src, class, attribute, colspan, href, target)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
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



---- PORTS AND SUBSCRIPTIONS ----


port listProducers : () -> Cmd msg


port listProducersOk : (JD.Value -> msg) -> Sub msg


port listProducersFail : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second Tick
        , listProducersOk ListProducersOk
        , listProducersFail ListProducersFail
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
    , selected : Bool
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


cypherglassBpAccount : String
cypherglassBpAccount =
    "cypherglasss"



---- UPDATE ----


type Msg
    = StartVoting
    | ConfirmVote
    | GenerateTransaction
    | ConfirmTransaction
    | Tick Time.Time
    | DeleteNotification String
    | ListProducersOk JD.Value
    | ListProducersFail String
    | ToggleBpSelection String
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
            , listProducers ()
            )

        ConfirmVote ->
            ( { model | step = EnterPk }, Cmd.none )

        GenerateTransaction ->
            ( { model | step = TransactionConfirmation }, Cmd.none )

        ConfirmTransaction ->
            ( { model | step = SuccessFinal }, Cmd.none )

        ToggleBpSelection account ->
            let
                producers =
                    model.producers
                        |> List.map
                            (\producer ->
                                if producer.account == account then
                                    { producer | selected = not producer.selected }
                                else
                                    producer
                            )
            in
                ( { model | producers = producers }, Cmd.none )

        ListProducersOk rawProducers ->
            case (JD.decodeValue producersDecoder rawProducers) of
                Ok producers ->
                    let
                        adjustedProducers =
                            calcAndSortProducers producers
                    in
                        ( { model
                            | producers = adjustedProducers
                            , isLoading = model.isLoading - 1
                          }
                        , Cmd.none
                        )

                Err err ->
                    addError model "listProducersFailParse" err

        ListProducersFail err ->
            addError model "listProducersFail" err

        DeleteNotification id ->
            let
                notifications =
                    model.notifications
                        |> List.filter (\notification -> notification.id /= id)
            in
                ( { model | notifications = notifications }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


calcAndSortProducers : List Producer -> List Producer
calcAndSortProducers producers =
    producers
        |> List.map
            (\producer ->
                if producer.account == cypherglassBpAccount then
                    { producer | selected = True }
                else
                    producer
            )
        |> List.sortWith
            (\a b ->
                if a.account == cypherglassBpAccount then
                    LT
                else if b.account == cypherglassBpAccount then
                    GT
                else
                    case compare a.totalVotes b.totalVotes of
                        LT ->
                            GT

                        EQ ->
                            EQ

                        GT ->
                            LT
            )


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


stringToFloat : JD.Decoder Float
stringToFloat =
    JD.string
        |> JD.andThen
            (\val ->
                JD.succeed
                    (case String.toFloat val of
                        Ok val ->
                            val

                        Err err ->
                            0
                    )
            )


producersDecoder : JD.Decoder (List Producer)
producersDecoder =
    JD.list producerDecoder


producerDecoder : JD.Decoder Producer
producerDecoder =
    JDP.decode Producer
        |> JDP.required "owner" JD.string
        |> JDP.required "total_votes" stringToFloat
        |> JDP.required "producer_key" JD.string
        |> JDP.required "is_active" isActiveNumToBool
        |> JDP.required "url" JD.string
        |> JDP.hardcoded 5000
        |> JDP.hardcoded 0.0
        |> JDP.hardcoded False



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
        , p [ class "has-margin-top logo" ] [ text "VOTING" ]
        , p [ class "has-margin-top" ] [ text "The safer and easiest way to vote for EOS Block Producers" ]
        , a [ class "button is-info has-margin-top", onClick StartVoting ] [ text "Start Voting Session" ]
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
                , span [ class "title-span is-hidden-mobile" ] [ text "VOTING" ]
                , span [ class "title-span is-hidden-tablet" ] [ text "VT" ]
                , isLoadingSpan
                ]
            , div [ class "navbar-menu" ]
                [ div [ class "navbar-end" ] [ text "" ]
                ]
            ]



-- eosnewyorkio	https://bp.eosnewyork.io	130,418,350,542,964,670
-- eoscanadacom	https://www.eoscanada.com	123,929,333,890,318,540


producerRow : Producer -> Html Msg
producerRow producer =
    let
        checker =
            if producer.selected then
                a
                    [ onClick (ToggleBpSelection producer.account)
                    , class "has-text-success"
                    ]
                    [ icon "check" False False ]
            else
                a
                    [ onClick (ToggleBpSelection producer.account)
                    , class "has-text-grey"
                    ]
                    [ icon "close" False False ]

        bpLink =
            a [ href producer.url, target "_blank" ] [ text producer.url ]
    in
        tr []
            [ td [] [ checker ]
            , td [] [ text producer.account ]
            , td [] [ bpLink ]
            , td [] [ text (toString producer.totalVotes) ]
            ]


producersList : List Producer -> Html Msg
producersList producers =
    let
        producersRows =
            if List.length producers > 0 then
                producers
                    |> List.map producerRow
            else
                [ tr []
                    [ td
                        [ colspan 4, class "has-text-centered" ]
                        [ text "Producers not loaded" ]
                    ]
                ]
    in
        table [ class "table has-margin-top is-striped is-hoverable is-fullwidth" ]
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


listBpsView : Model -> Html Msg
listBpsView model =
    let
        selectedProducers =
            model.producers
                |> List.filter (\p -> p.selected)

        selectedProducersCount =
            List.length selectedProducers

        currentProducer =
            List.head selectedProducers

        ( voteButtonClass, voteButtonTxt, voteAttr, voteOp ) =
            case currentProducer of
                Just producer ->
                    let
                        text =
                            if selectedProducersCount > 1 then
                                "Confirm Vote for " ++ (toString selectedProducersCount) ++ " BPs"
                            else
                                "Confirm Vote for " ++ producer.account
                    in
                        if selectedProducersCount > 30 then
                            ( "is-danger", "Max Vote Limit of 30 BPs has Passed", "disabled", NoOp )
                        else
                            ( "is-success", text, "autofocus", ConfirmVote )

                Nothing ->
                    ( "", "Select at Least one BP to Vote", "disabled", NoOp )

        voteButton =
            a
                [ class ("button " ++ voteButtonClass)
                , onClick voteOp
                , attribute voteAttr ""
                ]
                [ text voteButtonTxt ]
    in
        pageView model
            [ titleMenu "Producers List" [ voteButton ]
            , p [] [ text "Select the Block Producers you want to vote on the left first column. You can choose up to 30 Block Producers." ]
            , producersList model.producers
            , p [ class "has-text-centered has-margin-top" ] [ voteButton ]
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
