module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, string)
import Html.Attributes exposing (src)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (Model, Msg(..), Photo, Status(..), initialModel, update, urlPrefix, view)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag)
import Test.Html.Event as Event

expectation : Expectation
expectation =
    Expect.equal (1 + 1) 2


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok "(untitled)")


sliders : Test
sliders =
    describe "Slider sets the desired field in the Model"
        [ testSlider "SlideHue" SlideHue .hue
        , testSlider "SlideRipple" SlideRipple .ripple
        , testSlider "SlideNoise" SlideNoise .noise
        ]


testSlider : String -> (Int -> Msg) -> (Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos to render." <|
        \_ ->
            initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)

urlFuzzer: Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount

urlsFromCount: Int -> List String
urlsFromCount count =
    List.range 1 count
        |> List.map (\num -> String.fromInt num ++ ".png")

thumbnailRendered: String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", attribute (src (urlPrefix ++ url )) ]
        |> Query.count (Expect.atLeast 1)

photoFromUrl: String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = ""}

thumbnailsRendered: Test
thumbnailsRendered =
    fuzz urlFuzzer "URLs render as thumbnails" <|
        \urls ->
            let
                thumbnailChecks: List (Query.Single msg -> Expectation) 
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                |> view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks

thumbnailClicked: Test
thumbnailClicked =
    fuzz3 urlFuzzer string urlFuzzer "Clicking a thumbnail should select it" <|
        \urlsBefore urlToSelect urlsAfter ->
            let
                url =
                    urlToSelect ++ ".jpeg"

                photos =
                    (urlsBefore ++ url :: urlsAfter)
                        |> List.map photoFromUrl

                srcToClick =
                    urlPrefix ++ url
            in
            { initialModel | status = Loaded photos "" }
                |> view
                |> Query.fromHtml
                |> Query.find [ tag "img", attribute (src srcToClick) ]
                |> Event.simulate Event.click
                |> Event.expect (ClickedPhoto url)
