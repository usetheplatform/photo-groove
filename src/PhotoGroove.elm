module PhotoGroove exposing (main)

import Browser
import Html exposing (Html, div, h1, h3, button, img, text, input, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Http

type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String

type alias Model =
    { status: Status
    , chosenSize: ThumbnailSize
    }

type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error String)
type alias Photo =
    { url: String }

type ThumbnailSize
    = Small
    | Medium
    | Large

initialModel: Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

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
            ( { model | status = selectUrl photo.url model.status }, Cmd.none)

        GotPhotos (Ok responseStr) ->
            case String.split "," responseStr of
                        (firstUrl :: _) as urls ->
                            let
                                    photos =
                                        List.map Photo urls
                            in
                            ( { model | status = Loaded photos firstUrl }, Cmd.none)
                        [] ->
                            ( { model | status = Errored "0 photos found" }, Cmd.none)

        GotPhotos (Err _) ->
            ({ model | status = Errored "Server error!" }, Cmd.none)

initalCmd: Cmd Msg
initalCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list"
        , expect = Http.expectString GotPhotos
        }

urlPrefix: String
urlPrefix =
    "http://elm-in-action.com/"

view: Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize
                
            Loading ->
                []
            
            Errored errorMessage ->
                [ text ("Error " ++ errorMessage) ]

viewLoaded: List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise me!"]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map (viewSizeChooser chosenSize) [Small, Medium, Large])
        , div [ id "thumbnails", class (sizeToString chosenSize) ]
            (List.map (viewThumbnail selectedUrl) photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ selectedUrl)
            ]
            []
        ]

viewThumbnail: String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
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

sizeToString: ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"
        Medium ->
            "medium"
        Large ->
            "large"

selectUrl: String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url
        Loading ->
            status
        Errored _ ->
            status

main: Program () Model Msg
main =
    Browser.element
        { init = \_ -> (initialModel, initalCmd)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
