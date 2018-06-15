module Model exposing (..)

import Time
import Json.Decode as JD


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
    , pkAccount : Maybe String
    , showPkModal : Bool
    , transactionSignature : Maybe String
    , isLoading : Int
    , notifications : List Notification
    , currentTime : Time.Time
    , blockData : Maybe BlockData
    , isNetworkConnected : Bool
    , isOnlineConsent : Bool
    , expirationCounter : Int
    }


initialModel : Model
initialModel =
    { producers = []
    , step = Welcome
    , pk = Nothing
    , pkAccount = Nothing
    , showPkModal = False
    , transactionSignature = Nothing
    , isLoading = 0
    , notifications = []
    , currentTime = 0
    , blockData = Nothing
    , isNetworkConnected = False
    , isOnlineConsent = False
    , expirationCounter = 0
    }


cypherglassBpAccount : String
cypherglassBpAccount =
    "cypherglasss"


defaultTransactionExpiration : Int
defaultTransactionExpiration =
    600



--- UPDATE MSGs


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
    | UpdatePk String
    | UpdatePkAccount String
    | SignWithPk
    | SignTransactionOk String
    | SignTransactionFail String
    | ReInitialize
    | NoOp
