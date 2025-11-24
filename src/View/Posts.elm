module View.Posts exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck)
import Model exposing (Msg(..))
import Model.Post exposing (..)
import Time
import Util.Time
import Html exposing (thead)
import Http exposing (post)
import Model.PostsConfig exposing (PostsConfig, Change(..), sortOptions, sortToString, sortFromString, filterPosts)


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable config now posts =
    let
        visiblePosts =
            filterPosts config posts
    in
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Score" ]
                , th [] [ text "Title" ]
                , th [] [ text "Type" ]
                , th [] [ text "Posted" ]
                , th [] [ text "Link" ]
                ]
            ]
        , tbody [] (List.map (postRow now) visiblePosts)
        ]

    -- div [] []
    -- Debug.todo "postTable"


{-| Render a single table row for a post -}
postRow : Time.Posix -> Post -> Html Msg
postRow now post =
    let
        absoluteTime =
            Util.Time.formatTime Time.utc post.time

        relativeTime =
            case Util.Time.durationBetween post.time now of
                Just duration ->
                    " (" ++ Util.Time.formatDuration duration ++ ")"

                Nothing ->
                    ""

        linkText =
            case post.url of
                Just _ ->
                    "Link"

                Nothing ->
                    ""
    in
    tr []
        [ td [ class "post-score" ] [ text (String.fromInt post.score) ]
        , td [ class "post-title" ] [ text post.title ]
        , td [ class "post-type" ] [ text post.type_ ]
        , td [ class "post-time" ] [ text (absoluteTime ++ relativeTime) ]
        , td [ class "post-url" ]
            [ case post.url of
                Just url ->
                    a [ href url ] [ text linkText ]

                Nothing ->
                    text ""
            ]
        ]


{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    div []
        [
          label [ for "select-posts-per-page" ] [ text "Posts per page: " ]
        , select
            [ id "select-posts-per-page"
            , onInput
                (\v ->
                    ConfigChanged
                        (ChangePostsToShow (String.toInt v |> Maybe.withDefault config.postsToShow))
                )
            ]
            [ option [ value "10", selected (config.postsToShow == 10) ] [ text "10" ]
            , option [ value "25", selected (config.postsToShow == 25) ] [ text "25" ]
            , option [ value "50", selected (config.postsToShow == 50) ] [ text "50" ]
            ]

        , label [ for "select-sort-by" ] [ text "   Sort by: " ]
        , select
            [ id "select-sort-by"
            , onInput
                (\v ->
                    ConfigChanged
                        (ChangeSortBy (sortFromString v |> Maybe.withDefault config.sortBy))
                )
            ]
            (List.map
                (\opt ->
                    option
                        [ value (sortToString opt), selected (config.sortBy == opt) ]
                        [ text (sortToString opt) ]
                )
                sortOptions
            )

        , div []
            [ label []
                [ input
                    [ id "checkbox-show-job-posts"
                    , type_ "checkbox"
                    , checked config.showJobs
                    , onCheck (\flag -> ConfigChanged (ChangeShowJobs flag))
                    ]
                    []
                , text " Show job posts"
                ]
            ]

        , div []
            [ label []
                [ input
                    [ id "checkbox-show-text-only-posts"
                    , type_ "checkbox"
                    , checked config.showTextOnly
                    , onCheck (\flag -> ConfigChanged (ChangeShowTextOnly flag))
                    ]
                    []
                , text " Show text-only posts"
                ]
            ]
        ]
