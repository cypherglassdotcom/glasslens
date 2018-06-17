port module Main exposing (..)

import View exposing (view)
import Model exposing (..)
import Html exposing (Html)
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


port signTransaction : ( String, String, Int, Int, String, List String ) -> Cmd msg


port signTransactionOk : (String -> msg) -> Sub msg


port signTransactionFail : (String -> msg) -> Sub msg


port pushTransaction : String -> Cmd msg


port pushTransactionOk : (String -> msg) -> Sub msg


port pushTransactionFail : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second Tick
        , listProducersOk ListProducersOk
        , listProducersFail ListProducersFail
        , getBlockDataOk GetBlockDataOk
        , getBlockDataFail GetBlockDataFail
        , signTransactionOk SignTransactionOk
        , signTransactionFail SignTransactionFail
        , isNetworkOnline SetNetworkOnline
        , pushTransactionOk PushTransactionOk
        , pushTransactionFail PushTransactionFail
        ]



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                expirationCounter =
                    if model.expirationCounter > 0 then
                        model.expirationCounter - 1
                    else
                        model.expirationCounter

                transactionSignature =
                    if expirationCounter > 0 then
                        model.transactionSignature
                    else
                        Nothing
            in
                ( { model
                    | currentTime = time
                    , notifications = updateNotifications model.notifications model.currentTime
                    , expirationCounter = expirationCounter
                    , transactionSignature = transactionSignature
                  }
                , Cmd.none
                )

        SetNetworkOnline isOnline ->
            ( { model | isNetworkConnected = isOnline, isOnlineConsent = False }, Cmd.none )

        StartVoting ->
            ( { model
                | step = ListBps
                , pk = Nothing
                , pkAccount = Nothing
                , isLoading = model.isLoading + 1
              }
            , listProducers ()
            )

        ConfirmVote ->
            ( { model | step = EnterPk, isLoading = model.isLoading + 1 }, getBlockData () )

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

        UpdatePk text ->
            ( { model | pk = Just text }, Cmd.none )

        UpdatePkAccount text ->
            ( { model | pkAccount = Just text }, Cmd.none )

        TogglePkModal ->
            ( { model | showPkModal = not model.showPkModal, isOnlineConsent = False, pk = Nothing, pkAccount = Nothing }, Cmd.none )

        AcceptOnlineConsent ->
            ( { model | isOnlineConsent = True, pk = Nothing, pkAccount = Nothing }, Cmd.none )

        SignWithPk ->
            let
                selectedProducers =
                    model.producers
                        |> List.filter .selected
                        |> List.map .account
                        |> List.sort

                ( newModel, cmd ) =
                    case model.blockData of
                        Just blockData ->
                            case model.pk of
                                Just pk ->
                                    case model.pkAccount of
                                        Just pkAccount ->
                                            ( { model | isLoading = model.isLoading + 1 }, signTransaction ( pk, pkAccount, blockData.blockNum, blockData.refBlockPrefix, blockData.chainId, selectedProducers ) )

                                        Nothing ->
                                            addError model "invalidSignSubmission" "You must enter the Private Key Account to sign your voting transaction"

                                Nothing ->
                                    addError model "invalidSignSubmission" "You must enter the Private Key and Account Name to sign your voting transaction"

                        Nothing ->
                            addError model "invalidSignSubmission" "Cannot sign transaction with empty block data"
            in
                ( newModel, cmd )

        SignTransactionOk signature ->
            ( { model
                | transactionSignature = Just signature
                , pk = Nothing
                , pkAccount = Nothing
                , isLoading = model.isLoading - 1
                , expirationCounter = defaultTransactionExpiration
              }
            , Cmd.none
            )

        SignTransactionFail err ->
            addError model "signTransactionError" err

        PushTransaction ->
            if model.isNetworkConnected then
                case model.transactionSignature of
                    Just transactionSignature ->
                        ( { model | isLoading = model.isLoading + 1 }, pushTransaction transactionSignature )

                    Nothing ->
                        addError model "pushTransactionError" "Transaction is not signed, please restart your voting session."
            else
                addError model "pushTransactionOffline" "Ooops... You are still offline, please wait for your status to be ONLINE and then submit the transaction again"

        PushTransactionOk transactionId ->
            ( { model
                | step = SuccessFinal
                , transactionId = transactionId
                , isLoading = model.isLoading - 1
              }
            , Cmd.none
            )

        PushTransactionFail err ->
            addError model "pushTransactionError" err

        ReInitialize ->
            ( { initialModel | isNetworkConnected = model.isNetworkConnected }, Cmd.none )

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
