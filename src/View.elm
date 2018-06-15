module View exposing (..)

import Model exposing (..)
import Components exposing (icon, loadingIcon, titleMenu, modalCard, columns, passwordInput, fieldInput)
import Html exposing (Html, div, img, p, text, a, button, section, span, nav, tr, td, table, thead, th, li, ul, h1, strong, br, small)
import Html.Attributes exposing (src, class, attribute, colspan, href, target)
import Html.Events exposing (onClick)


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
                div [ class "has-margin-top" ]
                    [ passwordInput
                        model.isLoading
                        "Private Key"
                        (Maybe.withDefault "" model.pk)
                        "Enter your Private Key"
                        "key"
                        UpdatePk
                        False
                    , fieldInput
                        model.isLoading
                        "EOS Account Name"
                        (Maybe.withDefault "" model.pkAccount)
                        "Enter your EOS Account"
                        "user"
                        UpdatePkAccount
                        False
                    ]
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
                            [ h1 [ class "title" ] [ text "Voting Data" ]
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