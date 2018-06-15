port module Main exposing (..)

import Html exposing (Html, Attribute, text, div, button, section, h1, img, p, a, nav, span, i, table, thead, th, tr, td, h3, small, strong, br, ul, li, header, footer)
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


port isNetworkOnline : (Bool -> msg) -> Sub msg


port getBlockData : () -> Cmd msg


port getBlockDataOk : (JD.Value -> msg) -> Sub msg


port getBlockDataFail : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second Tick
        , listProducersOk ListProducersOk
        , listProducersFail ListProducersFail
        , getBlockDataOk GetBlockDataOk
        , getBlockDataFail GetBlockDataFail
        , isNetworkOnline SetNetworkOnline
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


type alias BlockData =
    { chainId : String
    , blockNum : Int
    , refBlockPrefix : Int
    }


type alias Model =
    { producers : List Producer
    , step : Step
    , pk : Maybe String
    , showPkModal : Bool
    , isLoading : Int
    , notifications : List Notification
    , currentTime : Time.Time
    , blockData : Maybe BlockData
    , isNetworkConnected : Bool
    , isOnlineConsent : Bool
    }


initialModel : Model
initialModel =
    { producers = []
    , step = Welcome
    , pk = Nothing
    , showPkModal = False
    , isLoading = 0
    , notifications = []
    , currentTime = 0
    , blockData = Nothing
    , isNetworkConnected = False
    , isOnlineConsent = False
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
    | GetBlockDataOk JD.Value
    | GetBlockDataFail String
    | ToggleBpSelection String
    | TogglePkModal
    | SetNetworkOnline Bool
    | AcceptOnlineConsent
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

        SetNetworkOnline isOnline ->
            ( { model | isNetworkConnected = isOnline, isOnlineConsent = False }, Cmd.none )

        StartVoting ->
            ( { model
                | step = ListBps
                , pk = Nothing
                , isLoading = model.isLoading + 1
              }
            , listProducers ()
            )

        ConfirmVote ->
            ( { model | step = EnterPk, isLoading = model.isLoading + 1 }, getBlockData () )

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

        GetBlockDataOk rawBlockData ->
            case (JD.decodeValue blockDataDecoder rawBlockData) of
                Ok blockData ->
                    ( { model
                        | blockData = Just blockData
                        , isLoading = model.isLoading - 1
                      }
                    , Cmd.none
                    )

                Err err ->
                    addError model "getBlockDataParseFail" err

        ListProducersFail err ->
            addError model "listProducersFail" err

        GetBlockDataFail err ->
            addError model "getBlockDataFail" err

        DeleteNotification id ->
            let
                notifications =
                    model.notifications
                        |> List.filter (\notification -> notification.id /= id)
            in
                ( { model | notifications = notifications }, Cmd.none )

        TogglePkModal ->
            ( { model | showPkModal = not model.showPkModal, isOnlineConsent = False, pk = Nothing }, Cmd.none )

        AcceptOnlineConsent ->
            ( { model | isOnlineConsent = True, pk = Nothing }, Cmd.none )

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


blockDataDecoder : JD.Decoder BlockData
blockDataDecoder =
    JDP.decode BlockData
        |> JDP.required "chain_id" JD.string
        |> JDP.required "block_num" JD.int
        |> JDP.required "ref_block_prefix" JD.int



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
        , p [ class "has-margin-top logo" ] [ text "LENS" ]
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
        ( networkStatusTxt, networkStatusClass, networkStatusIcon ) =
            if model.isNetworkConnected then
                ( "ONLINE", "has-text-success", "circle" )
            else
                ( "OFFLINE", "has-text-danger", "power-off" )

        networkSpan =
            span [ class (networkStatusClass ++ " network-status") ]
                [ text networkStatusTxt
                , icon networkStatusIcon False False
                ]

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
                , span [ class "title-span is-hidden-mobile" ] [ text "LENS" ]
                , span [ class "title-span is-hidden-tablet" ] [ text "LENS" ]
                , isLoadingSpan
                ]
            , div [ class "navbar-menu is-active" ]
                [ div [ class "navbar-end" ] [ networkSpan ]
                ]
            ]


disabledAttribute : Bool -> Attribute msg
disabledAttribute isDisabled =
    if isDisabled then
        attribute "disabled" "true"
    else
        attribute "data-empty" ""


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


selectedBpsList : List Producer -> Html msg
selectedBpsList producers =
    let
        items =
            producers
                |> List.filter (\p -> p.selected)
                |> List.map
                    (\p ->
                        li [] [ text p.account ]
                    )
    in
        ul [] items


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


columns : Bool -> List (Html Msg) -> Html Msg
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


pkModal : Model -> Html Msg
pkModal model =
    let
        pkForm =
            if model.isNetworkConnected && not model.isOnlineConsent then
                div []
                    [ p []
                        [ text "We detected that you are "
                        , span [ class "has-text-success" ] [ text " ONLINE" ]
                        , text ". We highly recommend you to go offline before entering your private key and signing the transaction."
                        ]
                    , a [ onClick AcceptOnlineConsent, class "has-text-danger" ] [ text "I understand the risks and I want to sign my vote online" ]
                    ]
            else
                div [] [ p [] [ text "Enter your Private Key: ____" ] ]
    in
        modalCard model.isLoading
            "Sign with Private Key"
            TogglePkModal
            [ div [ class "content" ]
                [ p [] [ text "Enter the Private Key in any Application is a delicated action and it deserves the maximum possible care." ]
                , p [] [ text "Please understand that right after we prepare your voting transaction, we destroy your private key and we never save it anywhere." ]
                , pkForm
                ]
            ]
            Nothing
            Nothing


enterPkView : Model -> Html Msg
enterPkView model =
    case model.blockData of
        Just blockData ->
            let
                modal =
                    if model.showPkModal then
                        pkModal model
                    else
                        text ""
            in
                pageView model
                    [ (columns
                        False
                        [ div []
                            [ h1 [ class "title" ] [ text "Block Data" ]
                            , p []
                                [ strong [] [ text "Current Block:" ]
                                , span [] [ text (toString blockData.blockNum) ]
                                , br [] []
                                ]
                            , p [ class "has-margin-top" ]
                                [ strong [] [ text "Selected Block Producers" ] ]
                            , selectedBpsList model.producers
                            ]
                        , div []
                            [ div [ class "has-text-centered" ]
                                [ h1 [ class "title" ] [ text "Sign Voting Transaction" ]
                                , a [ class "button is-info", onClick TogglePkModal ] [ text "Sign with Private Key" ]
                                ]
                            , div [ class "has-text-centered has-margin-top-2x" ]
                                [ small [] [ text "Hardware Wallets Coming Soon..." ]
                                , p [ class "has-margin-top" ] [ a [ class "button is-success", attribute "disabled" "" ] [ text "Sign with Ledger Nano" ] ]
                                , p [ class "has-margin-top" ]
                                    [ a [ class "button is-success", attribute "disabled" "" ] [ text "Sign with Trezor" ]
                                    ]
                                , p [ class "has-margin-top" ]
                                    [ a [ target "_blank", href "https://steemit.com/eos/@cypherglass/usd100k-eos-hardware-wallet-bounty" ]
                                        [ text "Learn More..." ]
                                    ]
                                ]
                            ]
                        ]
                      )
                    , modal
                    ]

        Nothing ->
            if model.isLoading > 0 then
                pageView model
                    [ p [ class "has-text-centered has-margin-top" ]
                        [ loadingIcon
                        , span [] [ text " Loading..." ]
                        ]
                    ]
            else
                pageView model
                    [ div [ class "has-text-centered has-margin-top" ]
                        [ p [] [ text "Fail to Load EOS Blockchain Data" ]
                        , a [ class "button has-margin-top", onClick ConfirmVote ]
                            [ icon "refresh" False False
                            , span [] [ text "Retry" ]
                            ]
                        ]
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
