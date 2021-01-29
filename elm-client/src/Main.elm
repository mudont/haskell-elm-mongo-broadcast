port module Main exposing (..)

import Browser
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D
import Dict
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

type alias Quote = { 
    instrumentSymbol: String, price: Float
  }
type alias Model =
  { draft : String
  , nQuotes: Int
  , quotes : Dict.Dict String Quote
  , tableState : Table.State 
  }


init : () -> ( Model, Cmd Msg )
init flags =
  ( {
        draft = "",
        nQuotes = 0,
        quotes = Dict.empty, 
        tableState = Table.initialSort "instrumentSymbol"
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = DraftChanged String
  | Send
  | Recv String
  | SetTableState Table.State


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

    Recv quote ->
      ( processQuote quote model
      , Cmd.none
      )

    SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

processQuote : String -> Model -> Model
processQuote quote model =
    let q = getQuote quote 
        qs = model.quotes
        qs2 =
            if Dict.member q.instrumentSymbol qs 
            then  Dict.update q.instrumentSymbol (\_ -> Just q) qs
            else Dict.insert q.instrumentSymbol q qs 
    in {model | quotes = qs2, nQuotes = model.nQuotes + 1}
quoteDecoder : D.Decoder Quote
quoteDecoder = D.map2 Quote 
    (D.field "instrumentSymbol" D.string) 
    (D.field "price" D.float)

getQuote : String -> Quote
getQuote msg = case D.decodeString quoteDecoder msg of
    Err _ -> Quote "Bad" 0.0
    Ok q -> q
    

-- SUBSCRIPTIONS


-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--
subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver Recv



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Quotes" ]
    , text (String.fromInt model.nQuotes)
    , (Table.view config model.tableState (Dict.values model.quotes))
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


-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
  D.field "key" D.string
    |> D.andThen (\key -> if key == "Enter" then D.succeed msg else D.fail "some other key")
    
