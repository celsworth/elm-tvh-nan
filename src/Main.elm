module Main exposing (main)

import Browser
import DateFormat as DF
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as Pipeline exposing (required)
import Task
import Time


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Flags =
    {}


type alias Model =
    { time : Time.Posix
    , zone : Time.Zone
    , channels : List Channel
    , events : List Event
    }


type alias Channel =
    { name : String
    , number : Int
    , uuid : String
    , icon : String
    }


type alias Event =
    { title : String
    , subtitle : Maybe String
    , episode : Maybe String
    , channelUuid : String
    , startTime : Int
    , stopTime : Int
    }


type Msg
    = NoOp
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | GotChannels (Result Http.Error (List Channel))
    | GetEpg
    | GotEpg (Result Http.Error (List Event))



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { time = Time.millisToPosix 0
      , zone = Time.utc
      , channels = []
      , events = []
      }
    , Cmd.batch
        [ getChannels
        , Task.perform AdjustTimeZone Time.here
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 500 Tick


isEventInPast : Model -> Event -> Bool
isEventInPast model event =
    event.stopTime < (Time.posixToMillis model.time // 1000)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        shouldGetEpg =
            if List.any (isEventInPast model) model.events then
                getEpg

            else
                Cmd.none
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }, shouldGetEpg )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        GotChannels (Ok newChannels) ->
            ( { model | channels = newChannels }, getEpg )

        GotChannels (Err _) ->
            -- display error?
            ( model, Cmd.none )

        GetEpg ->
            ( model, getEpg )

        GotEpg (Ok newEvents) ->
            ( { model | events = newEvents }, Cmd.none )

        GotEpg (Err _) ->
            -- display error?
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "elm-tvh-nan"
    , body = [ channelTable model ]
    }


channelTable : Model -> Html Msg
channelTable model =
    table [] <|
        List.map (channelTableRow model) model.channels


channelTableRow : Model -> Channel -> Html Msg
channelTableRow model channel =
    let
        channelEvents =
            List.filter (\e -> e.channelUuid == channel.uuid) model.events
    in
    tr []
        [ channelTableIconCell channel
        , channelTableNowCell model channelEvents channel
        , channelTableNextCell model channelEvents channel
        ]


channelTableIconCell : Channel -> Html Msg
channelTableIconCell channel =
    td []
        [ img [ title channel.name, src channel.icon ] []
        ]


channelTableNowCell : Model -> List Event -> Channel -> Html Msg
channelTableNowCell model events channel =
    let
        now =
            events
                |> List.head
                |> Maybe.map (eventTags True model)
                |> Maybe.withDefault []
    in
    td [] now


channelTableNextCell : Model -> List Event -> Channel -> Html Msg
channelTableNextCell model events channel =
    let
        next1 =
            events
                |> List.drop 1
                |> List.head
                |> Maybe.map (eventTags False model)
                |> Maybe.withDefault []

        next2 =
            events
                |> List.drop 2
                |> List.head
                |> Maybe.map (eventTags False model)
                |> Maybe.withDefault []
    in
    td [ class "next" ] (next1 ++ [ br [] [] ] ++ next2)


eventTags : Bool -> Model -> Event -> List (Html Msg)
eventTags now model event =
    let
        elapsed =
            toFloat <| (Time.posixToMillis model.time // 1000) - event.startTime

        duration =
            toFloat <| event.stopTime - event.startTime

        pct =
            elapsed / duration * 100.0

        maybeProgressBar =
            if now then
                Just <| eventProgressBar pct

            else
                Nothing
    in
    List.filterMap identity
        [ Just <| startTimeTag model event.startTime
        , Just <| text event.title
        , episodeTag event.episode event.subtitle
        , maybeProgressBar
        ]


eventProgressBar : Float -> Html Msg
eventProgressBar donePercent =
    div [ class "progress_container" ]
        [ div
            [ class "progress_fill"
            , style "width" (String.fromFloat donePercent ++ "%")
            ]
            []
        ]


startTimeTag : Model -> Int -> Html Msg
startTimeTag model startTime =
    b []
        [ text <|
            DF.format
                [ DF.hourMilitaryFixed, DF.text ":", DF.minuteFixed ]
                model.zone
                (Time.millisToPosix (startTime * 1000))
        ]


episodeTag : Maybe String -> Maybe String -> Maybe (Html Msg)
episodeTag episode subtitle =
    let
        parts =
            List.filterMap identity [ episode, subtitle ]
    in
    if List.isEmpty parts then
        Nothing

    else
        Just <| em [] [ text <| "(" ++ String.join " - " parts ++ ")" ]



-- TVHEADEND JSON INTEGRATION


getChannels : Cmd Msg
getChannels =
    Http.get
        { url = "/api/channel/grid"
        , expect = Http.expectJson GotChannels channelsDecoder
        }


channelsDecoder : JD.Decoder (List Channel)
channelsDecoder =
    JD.field "entries" (JD.list channelDecoder)
        |> JD.map (List.sortBy .number)


channelDecoder : JD.Decoder Channel
channelDecoder =
    JD.succeed Channel
        |> Pipeline.required "name" JD.string
        |> Pipeline.required "number" JD.int
        |> Pipeline.required "uuid" JD.string
        |> Pipeline.required "icon_public_url" JD.string


getEpg =
    Http.get
        { url = "/api/epg/events/grid?limit=500"
        , expect = Http.expectJson GotEpg epgDecoder
        }


epgDecoder : JD.Decoder (List Event)
epgDecoder =
    JD.field "entries" (JD.list eventDecoder)


eventDecoder : JD.Decoder Event
eventDecoder =
    JD.succeed Event
        |> Pipeline.required "title" JD.string
        |> Pipeline.optional "subtitle" (JD.map Just JD.string) Nothing
        |> Pipeline.custom episodeDecoder
        |> Pipeline.required "channelUuid" JD.string
        |> Pipeline.required "start" JD.int
        |> Pipeline.required "stop" JD.int


episodeDecoder : JD.Decoder (Maybe String)
episodeDecoder =
    JD.succeed (Maybe.map2 episodeString)
        |> Pipeline.optional "seasonNumber" (JD.map Just JD.int) Nothing
        |> Pipeline.optional "episodeNumber" (JD.map Just JD.int) Nothing


episodeString : Int -> Int -> String
episodeString seasonNumber episodeNumber =
    String.fromInt seasonNumber ++ "x" ++ String.fromInt episodeNumber
