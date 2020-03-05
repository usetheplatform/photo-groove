port module PhotoGroove exposing (main)

import Browser
import Html exposing (Html, Attribute, node, div, h1, h3, button, img, text, input, label, canvas)
import Html.Attributes exposing (class, classList, id, src, type_, name, checked, title)
import Html.Events exposing (on, onClick)
import Random
import Http
import Json.Encode
import Json.Decode exposing (Decoder, at, list, string, int, succeed)
import Json.Decode.Pipeline exposing (optional, required)

type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String

type alias Model =
    { status: Status
    , chosenSize: ThumbnailSize
    , hue: Int
    , ripple: Int
    , noise: Int
    , activity: String
    }

type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | GotActivity String
    | SlideHue Int
    | SlideRipple Int
    | SlideNoise Int
type alias Photo =
    { url: String
    , size: Int
    , title: String
    }

port setFilters : FilterOptions -> Cmd msg

-- could be decoded using Json.Decode.decodeValue
-- port activityChanges: (Value -> msg) -> Sub msg
port activityChanges: (String -> msg) -> Sub msg

type alias FilterOptions =
    { url: String
    , filters: List { name: String, amount: Float }
    }

type ThumbnailSize
    = Small
    | Medium
    | Large

initialModel: Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
    , activity = ""
    }

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    (model, Cmd.none)

                Loading ->
                    (model, Cmd.none)
                
                Errored _ ->
                    (model, Cmd.none)
        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        GotPhotos (Ok photos) ->
            case photos of
                x :: _ ->
                    applyFilters { model | status = Loaded photos x.url }
                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )
        GotPhotos (Err _) ->
            ({ model | status = Errored "Server error!" }, Cmd.none)

        SlideHue hue ->
            applyFilters { model | hue = hue }
        
        SlideRipple ripple ->
            applyFilters { model | ripple = ripple }
        
        SlideNoise noise ->
            applyFilters { model | noise = noise }

        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )
applyFilters: Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded _ selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]
                url =
                    urlPrefix  ++ "large/" ++ selectedUrl
                
                cmd =
                    setFilters { url = url, filters = filters }
            in
            (model, cmd)
        
        Loading ->
            (model, Cmd.none)
        
        Errored _ ->
            (model, Cmd.none)

initalCmd: Cmd Msg
initalCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }

urlPrefix: String
urlPrefix =
    "http://elm-in-action.com/"

view: Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize model
                
            Loading ->
                []
            
            Errored errorMessage ->
                [ text ("Error " ++ errorMessage) ]

viewLoaded: List Photo -> String -> ThumbnailSize -> Model -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize model =
        [ h1 [ class "title" ] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe
            , class "surprise-me"
            ]
            [ text "Surprise me!"]
        , div [ class "activity" ] [ text model.activity ]
        , div [ class "filters" ]
            [ viewFilter SlideHue "Hue" model.hue
            , viewFilter SlideRipple "Ripple" model.ripple
            , viewFilter SlideNoise "Noise" model.noise
            ]
        , div [ class "sizes" ]
            [ h3 [] [ text "Thumbnail Size:" ]
            , div [ id "choose-size" ]
            (List.map (viewSizeChooser chosenSize) [Small, Medium, Large])
            ]
        , div [ id "thumbnails", class (sizeToString chosenSize) ]
            (List.map (viewThumbnail selectedUrl) photos)
        , canvas
            [ id "main-canvas"
            , class "large"
            ]
            []
        ]

viewThumbnail: String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ "KB]")
        , classList [ ("selected", selectedUrl == thumb.url) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []

viewSizeChooser: ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser selectedSize size =
    label []
        [ input [type_ "radio", name "size", checked (size == selectedSize), onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]

viewFilter: (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider"]
        [ label [] [ text name ]
        , rangeSlider
            [ Html.Attributes.max "11"
            , Html.Attributes.property "val" (Json.Encode.int magnitude )
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]

sizeToString: ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"
        Medium ->
            "medium"
        Large ->
            "large"

photoDecoder: Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"

selectUrl: String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url
        Loading ->
            status
        Errored _ ->
            status

main: Program Float Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init: Float -> (Model,  Cmd Msg)
init flags =
    let
        activity =
            "Initializing Past v" ++ String.fromFloat flags
    in
    ( { initialModel | activity = activity }, initalCmd )

subscriptions : Model -> Sub Msg
subscriptions _ =
    activityChanges GotActivity

rangeSlider: List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children

onSlide: (Int -> msg) -> Attribute msg
onSlide toMsg =
    at [ "detail", "userSlideTo" ] int
        |> Json.Decode.map toMsg
        |> on "slide"
