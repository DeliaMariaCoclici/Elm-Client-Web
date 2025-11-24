module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time



type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


{-|

    sortFromString "Score" --> Just Score

    sortFromString "Invalid" --> Nothing

    sortFromString "Title" --> Just Title

-}
sortFromString : String -> Maybe SortBy
sortFromString str =
    case str of
        "Score" ->
            Just Score

        "Title" ->
            Just Title

        "Posted" ->
            Just Posted

        "None" ->
            Just None

        _ ->
            Nothing

    -- Debug.todo "sortFromString"


sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ

            


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }


defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True


{-| A type that describes what option changed and how
-}
type Change
    = ChangePostsToShow Int
    | ChangeSortBy SortBy
    | ChangeShowJobs Bool
    | ChangeShowTextOnly Bool


{-| Given a change and the current configuration, return a new configuration with the changes applied
-}
applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges change config =
    case change of
        ChangePostsToShow n ->
            { config | postsToShow = n }

        ChangeSortBy s ->
            { config | sortBy = s }

        ChangeShowJobs b ->
            { config | showJobs = b }

        ChangeShowTextOnly b ->
            { config | showTextOnly = b }


{-| Given the configuration and a list of posts, return the relevant subset of posts according to the configuration

Relevant local functions:

  - sortToCompareFn

Relevant library functions:

  - List.sortWith

-}
filterPosts : PostsConfig -> List Post -> List Post
filterPosts config posts =
    posts
        |> List.filter (\p ->
            (config.showTextOnly || p.url /= Nothing)
                && (config.showJobs || p.type_ /= "job")
        )
        |> List.take config.postsToShow
        |> List.sortWith (sortToCompareFn config.sortBy)

    -- Debug.todo "filterPosts"
