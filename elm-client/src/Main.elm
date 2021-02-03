port module Main exposing (..)

import Browser
import Dict
import Env
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D exposing (Decoder, float, int, string)
import Json.Decode.Pipeline as DP
import List as L
import Maybe
import Round as R
import Table



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- MODEL


type alias Quote =
    { instrumentSymbol : String
    , price : Float
    }


type alias Security =
    { securitySymbol : String
    , askSizes : List Int
    , askPrices : List Float
    , bidSizes : List Int
    , bidPrices : List Float
    }


type DBObject
    = QuoteObj Quote
    | SecurityObj Security


type alias Model =
    { draft : String
    , nQuotes : Int
    , nSecurities : Int
    , quotes : Dict.Dict String Quote
    , securities : Dict.Dict String Security
    , quoteTableState : Table.State
    , secTableState : Table.State
    }


type Msg
    = DraftChanged String
    | Send
    | Recv String
    | SetTableState Table.State
    | SetSecTableState Table.State


init : () -> ( Model, Cmd Msg )
init flags =
    ( { draft = ""
      , nQuotes = 0
      , nSecurities = 0
      , quotes = Dict.empty
      , securities = Dict.empty
      , quoteTableState = Table.initialSort "instrumentSymbol"
      , secTableState = Table.initialSort "securitySymbol"
      }
    , Cmd.none
    )



-- UPDATE
-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DraftChanged draft ->
            ( { model | draft = draft }
            , Cmd.none
            )

        Send ->
            ( { model | draft = "" }
            , sendMessage model.draft
            )

        Recv rawObj ->
            ( processRawObj rawObj model
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | quoteTableState = newState }
            , Cmd.none
            )

        SetSecTableState newState ->
            ( { model | secTableState = newState }
            , Cmd.none
            )


processRawObj : String -> Model -> Model
processRawObj rawObj model =
    let
        o =
            getObj rawObj
    in
    case o of
        SecurityObj s ->
            let
                ss =
                    model.securities

                ss2 =
                    if Dict.member s.securitySymbol ss then
                        Dict.update s.securitySymbol (\_ -> Just s) ss

                    else
                        Dict.insert s.securitySymbol s ss
            in
            { model | securities = ss2, nSecurities = model.nSecurities + 1 }

        QuoteObj q ->
            let
                qs =
                    model.quotes

                qs2 =
                    if Dict.member q.instrumentSymbol qs then
                        Dict.update q.instrumentSymbol (\_ -> Just q) qs

                    else
                        Dict.insert q.instrumentSymbol q qs
            in
            { model | quotes = qs2, nQuotes = model.nQuotes + 1 }


quoteDecoder : D.Decoder Quote
quoteDecoder =
    D.map2 Quote
        (D.field "instrumentSymbol" D.string)
        (D.field "price" D.float)


securityDecoder : D.Decoder Security
securityDecoder =
    D.succeed Security
        |> DP.required "securitySymbol" D.string
        |> DP.required "askSizes" (D.list D.int)
        |> DP.required "askPrices" (D.list D.float)
        |> DP.required "bidSizes" (D.list D.int)
        |> DP.required "bidPrices" (D.list D.float)


objTypeDecoder : D.Decoder String
objTypeDecoder =
    D.field "collType" D.string


getObj : String -> DBObject
getObj msg =
    case D.decodeString objTypeDecoder msg of
        Ok objType ->
            case objType of
                "quote" ->
                    case D.decodeString quoteDecoder msg of
                        Err _ ->
                            QuoteObj (Quote "BadQ" 0.0)

                        Ok q ->
                            QuoteObj q

                "security" ->
                    case D.decodeString securityDecoder msg of
                        Err _ ->
                            SecurityObj (Security "BadS" [] [] [] [])

                        Ok s ->
                            SecurityObj s

                _ ->
                    QuoteObj (Quote "BadQ" 0.0)

        _ ->
            QuoteObj (Quote "BadQ" 0.0)



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.js file to see how this is hooked up to a
-- WebSocket.
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Quotes" ]
        , text (String.fromInt model.nQuotes)
        , Table.view config model.quoteTableState (Dict.values model.quotes)
        , h3 [] [ text "Securities" ]
        , text (String.fromInt model.nSecurities)
        , Table.view configSec model.secTableState (Dict.values model.securities)
        ]


config : Table.Config Quote Msg
config =
    Table.config
        { toId = .instrumentSymbol
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Symbol" .instrumentSymbol
            , Table.stringColumn "Price" (\q -> R.round 4 q.price)
            ]
        }


configSec : Table.Config Security Msg
configSec =
    Table.config
        { toId = .securitySymbol
        , toMsg = SetSecTableState
        , columns =
            [ Table.stringColumn "Symbol" .securitySymbol
            , Table.stringColumn "Bid Sizes" (\s -> catInts s.bidSizes)
            , Table.stringColumn "Bid Prices" (\s -> catFloats s.bidPrices)
            , Table.stringColumn "Ask Prices" (\s -> catFloats s.askPrices)
            , Table.stringColumn "Ask Sizes" (\s -> catInts s.askSizes)
            ]
        }


catFloats : List Float -> String
catFloats =
    L.foldl (\p agg -> agg ++ "|" ++ R.round 2 p) ""


catInts : List Int -> String
catInts =
    L.foldl (\p agg -> agg ++ "|" ++ String.fromInt p) ""



-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" then
                    D.succeed msg

                else
                    D.fail "some other key"
            )
