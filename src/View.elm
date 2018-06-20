module View exposing (..)

import Model exposing (..)
import Components exposing (icon, loadingIcon, titleMenu, modalCard, columns, passwordInput, fieldInput, disabledAttribute)
import Html exposing (Html, div, img, p, text, a, button, section, span, nav, tr, td, table, thead, th, li, ul, h1, strong, br, small, pre)
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

        SuccessFinal ->
            successView model


welcomeView : Model -> Html Msg
welcomeView model =
    div [ class "welcome" ]
        [ img [ src "assets/logo-cg.svg" ] []
        , p [ class "has-margin-top logo" ] [ text "LENS" ]
        , p [ class "has-margin-top" ]
            [ text "The safer and easiest way to vote for EOS Block Producers."
            , br [] []
            , text "This tool will not store your EOS private key on your computer or transmit it over the internet."
            ]
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
                    [ icon "check-square-o" False False ]
            else
                a
                    [ onClick (ToggleBpSelection producer.account)
                    , class "has-text-grey"
                    ]
                    [ icon "square-o" False False ]

        bpLink =
            a [ href producer.url, target "_blank" ] [ text producer.url ]
    in
        tr []
            [ td [] [ checker ]
            , td [] [ text producer.account ]
            , td [] [ bpLink ]
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
                        [ colspan 3, class "has-text-centered" ]
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


pkSection : Model -> Html Msg
pkSection model =
    let
        ( signSubmit, signDisabled ) =
            if model.isLoading > 0 || model.pk == Nothing || model.pkAccount == Nothing then
                ( NoOp, True )
            else
                ( SignWithPk, False )

        submitButton =
            p [ class "has-text-centered has-margin-top" ]
                [ a
                    [ class "button is-success"
                    , onClick signSubmit
                    , disabledAttribute signDisabled
                    ]
                    [ text "Sign Transaction" ]
                ]

        intro =
            if model.transactionSignature == Nothing then
                (div []
                    [ p [] [ text "You will be prompted to enter your private key on the next screen." ]
                    , p [] [ text "Please insure that you are in a private place, where nobody can see your computer screen, and that your computer is not connected to a network via cable or wireless." ]
                    ]
                )
            else
                text ""

        statusText =
            if model.transactionSignature /= Nothing && not model.isNetworkConnected then
                p [ class "has-margin-top" ]
                    [ text "You are still "
                    , strong [ class "has-text-danger" ] [ text "OFFLINE" ]
                    , text ", please go Online to Submit your Vote!"
                    ]
            else
                text ""

        pkForm =
            if model.transactionSignature /= Nothing then
                div []
                    [ strong [ class "has-text-success" ] [ text "Your transaction was signed and your Private Key was already destroyed from the session safely." ]
                    , statusText
                    , div [ class "has-text-centered has-margin-top" ]
                        [ p [] [ text ("Your voting transaction is prepared and will expire in " ++ (toString model.expirationCounter) ++ " seconds.") ]
                        , p [ class "has-margin-top" ]
                            [ a [ class "button is-success", disabledAttribute (model.isLoading > 0), onClick PushTransaction ] [ text "Submit Vote" ] ]
                        , p [ class "has-margin-top" ]
                            [ a [ class "button is-danger", disabledAttribute (model.isLoading > 0), onClick ReInitialize ] [ text "Cancel and Restart" ] ]
                        ]
                    ]
            else if model.isNetworkConnected && not model.isOnlineConsent then
                div []
                    [ intro
                    , p [ class "has-margin-top" ]
                        [ text "Cypherglass Lens has detected that your computer is still "
                        , span [ class "has-text-success" ] [ text " ONLINE" ]
                        , text ". We highly recommend you go offline before entering your private key on the next screen and signing the transaction."
                        ]
                    , p [] [ a [ onClick ToggleDisconnectionModal ] [ text "Disconnection Instructions" ] ]
                    , p [ class "has-text-centered" ] [ a [ onClick AcceptOnlineConsent, class "button is-danger" ] [ text "I understand the risks and I want to sign my vote online" ] ]
                    , backButton
                    ]
            else
                div [ class "has-margin-top" ]
                    [ p [] [ text "Please enter your private key below." ]
                    , p [] [ text "Cypherglass Lens will never store your private key.  You will enter it once below while you are offline to create a transaction.  Once the transaction is ready to send, no record of your private key will exist on this computer." ]
                    , passwordInput
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
                    , submitButton
                    , backButton
                    ]

        backButton =
            p [ class "has-text-centered has-margin-top" ]
                [ a [ class "button", onClick TogglePkSection ]
                    [ text "Go Back and Choose Another Option" ]
                ]
    in
        div []
            [ h1 [ class "title" ] [ text "Sign with Private Key" ]
            , div [ class "content" ] [ pkForm ]
            ]


disconnectionModal : Html Msg
disconnectionModal =
    modalCard 0
        "Disconnection Instructions"
        ToggleDisconnectionModal
        [ div [ class "content" ]
            [ p [] [ text "WIRED: To disconnect your PC or Mac from a wired network, simply disconnect the cable." ]
            , p [] [ text "PC: To disconnect your PC from a wireless network click on the wireless icon in the lower right of your system tray and then click disconnect." ]
            , p [] [ text "MAC: To disconnect your Mac from a wireless network click on the wireless icon in the upper right of your system tray and then click in \"Turn Wi-Fi Off\"." ]
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
                    if model.showDisconnectionModal then
                        disconnectionModal
                    else
                        text ""

                signatureContent =
                    if model.showPkSection then
                        pkSection model
                    else
                        div []
                            [ div [ class "has-text-centered" ]
                                [ h1 [ class "title" ] [ text "Sign Voting Transaction" ]
                                , a [ class "button is-info", onClick TogglePkSection ] [ text "Sign with Private Key" ]
                                ]
                            , div [ class "has-text-centered has-margin-top-2x" ]
                                [ small []
                                    [ text "Hardware Wallets Coming Soon... "
                                    , a [ target "_blank", href "https://steemit.com/eos/@cypherglass/usd100k-eos-hardware-wallet-bounty" ]
                                        [ text "Learn More" ]
                                    ]
                                , p [ class "has-margin-top" ]
                                    [ a [ class "button is-success", attribute "disabled" "" ] [ text "Sign with Ledger Nano" ] ]
                                , p [ class "has-margin-top" ]
                                    [ a [ class "button is-success", attribute "disabled" "" ] [ text "Sign with Trezor" ]
                                    ]
                                , p [ class "has-margin-top" ]
                                    []
                                , p [ class "has-margin-top" ]
                                    [ a [ class "button is-danger", onClick ReInitialize ] [ text "Cancel and Restart" ] ]
                                ]
                            ]
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
                        , signatureContent
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


successView : Model -> Html Msg
successView model =
    let
        producers =
            model.producers
                |> List.filter .selected
                |> List.map .account
                |> String.join " "

        txLink =
            "https://eosflare.io/tx/" ++ model.transactionId

        lensLink =
            "cypherglass.com/lens"

        socialTxt =
            "I supported the EOS community and voted for an EOS Block Producer using @CypherglassBP Lens! Check it out: " ++ lensLink

        twitterLink =
            "https://twitter.com/intent/tweet?text=" ++ socialTxt

        facebookLink =
            "https://www.facebook.com/sharer.php?u=" ++ lensLink
    in
        pageView model
            [ div [ class "has-text-centered" ]
                [ h1 [ class "title" ] [ text "Thanks for Voting with Cypherglass Lens!" ]
                , p [] [ text "You successfully voted for: ", pre [] [ text producers ] ]
                , p [ class "has-margin-top" ]
                    [ text "Your Transaction Id is "
                    , strong [] [ text model.transactionId ]
                    , text " - "
                    , a [ href txLink, target "_blank" ]
                        [ text "Check it here!" ]
                    ]
                , p [ class "has-margin-top" ]
                    [ text "Spread the word of your "
                    , strong [] [ text "AMAZING EOS Civic Duty" ]
                    , text " to the World!"
                    ]
                , p [ class "has-margin-top" ]
                    [ a [ href twitterLink, target "_blank", class "button is-info" ]
                        [ span [ class "icon" ] [ icon "twitter" False False ]
                        , span [] [ text "Share on Twitter" ]
                        ]
                    , span [] [ text " " ]
                    , a [ href facebookLink, target "_blank", class "button is-link" ]
                        [ span [ class "icon" ] [ icon "facebook" False False ]
                        , span [] [ text "Share on Facebook" ]
                        ]
                    ]
                , p [ class "has-margin-top-2x" ]
                    [ p [] [ text "Do you have more accounts to vote?" ]
                    , a [ class "has-margin-top button is-primary", onClick ReInitialize ] [ text "New Voting Session" ]
                    ]
                ]
            ]
