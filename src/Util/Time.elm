module Util.Time exposing (..)

import Time

type Date
    = Date { year : Int, month : Time.Month, day : Int }


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


posixToDate : Time.Zone -> Time.Posix -> Date
posixToDate tz time =
    let
        year =
            Time.toYear tz time

        month =
            Time.toMonth tz time

        day =
            Time.toDay tz time
    in
    Date { year = year, month = month, day = day }


--| Formats a `Date` instance.
--    import Time
--
--    formatDate (Date { year = 2022, month = Time.Apr, day =  4 }) {- ignore -} --> "2022 Apr 04"
--
--    formatDate (Date { year = 2022, month = Time.Jan, day = 12 }) {- ignore -} --> "2022 Jan 12"

formatDate : Date -> String
formatDate (Date date) =
    let
        year =
            String.fromInt date.year

        month =
            monthToString date.month

        day =
            String.fromInt date.day |> String.padLeft 2 '0'
    in
    year ++ " " ++ month ++ " " ++ day


formatTime : Time.Zone -> Time.Posix -> String
formatTime tz time =
    let
        date =
            posixToDate tz time

        hour =
            Time.toHour tz time |> String.fromInt |> String.padLeft 2 '0'

        minute =
            Time.toMinute tz time |> String.fromInt |> String.padLeft 2 '0'
    in
    formatDate date ++ " " ++ hour ++ ":" ++ minute


type alias Duration =
    { seconds : Int
    , minutes : Int
    , hours : Int
    , days : Int
    }


{-| Calculates the amount of time that passed between two dates.

The first date (t1) must be **before** the second date (t2), if this not the case, the function should return `Nothing`.

Relevant library functions:

  - Use Time.posixToMillis

```
import Time

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (1000)) --> Just (Duration 1 0 0 0)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (60 * 1000)) --> Just (Duration 0 1 0 0)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (60 * 60 * 1000)) --> Just (Duration 0 0 1 0)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (24 * 60 * 60 * 1000)) --> Just (Duration 0 0 0 1)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (24 * 60 * 60 * 1000 + 1000)) --> Just (Duration 1 0 0 1)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (4 * 24 * 60 * 60 * 1000 + 3 * 60 * 60 * 1000 + 2 * 60 * 1000 + 1000)) --> Just (Duration 1 2 3 4)

durationBetween (Time.millisToPosix 1000) (Time.millisToPosix 0) --> Nothing

durationBetween (Time.millisToPosix 1000) (Time.millisToPosix 1000) --> Nothing
```

-}

--- dropRight 2 [1,2,3,4,5] = [1,2,3]
dropRight : Int -> List a -> List a
dropRight n list =
    list
        |> List.reverse
        |> List.drop n
        |> List.reverse

durationBetween : Time.Posix -> Time.Posix -> Maybe Duration
durationBetween t1 t2 =
    let
        ms1 = Time.posixToMillis t1
        ms2 = Time.posixToMillis t2
        diff = ms2 - ms1
    in
    if diff <= 0 then
        Nothing
    else
        let
            secondsTotal = diff // 1000

            days = secondsTotal // (24 * 60 * 60)
            remAfterDays = modBy (24 * 60 * 60) secondsTotal

            hours = remAfterDays // (60 * 60)
            remAfterHours = modBy (60 * 60) remAfterDays

            minutes = remAfterHours // 60
            seconds = modBy 60 remAfterHours
        in
        Just { seconds = seconds, minutes = minutes, hours = hours, days = days }

    -- Debug.todo "durationBetween"


{-| Format a `Duration` as a human readable string

    formatDuration (Duration 1 0 0 0) --> "1 second ago"

    formatDuration (Duration 2 0 0 0) --> "2 seconds ago"

    formatDuration (Duration 0 1 0 0) --> "1 minute ago"

    formatDuration (Duration 0 0 2 0) --> "2 hours ago"

    formatDuration (Duration 0 0 0 3) --> "3 days ago"

    formatDuration (Duration 0 1 1 1) --> "1 day 1 hour 1 minute ago"

    formatDuration (Duration 0 47 6 2) --> "2 days 6 hours 47 minutes ago"

    formatDuration (Duration 0 30 0 1) --> "1 day 30 minutes ago"

-}
formatDuration : Duration -> String
formatDuration duration =
    let
        pluralize n unit =
            if n == 1 then
                String.fromInt n ++ " " ++ unit
            else
                String.fromInt n ++ " " ++ unit ++ "s"

        parts =
            List.filterMap identity
                [ if duration.days > 0 then Just (pluralize duration.days "day") else Nothing
                , if duration.hours > 0 then Just (pluralize duration.hours "hour") else Nothing
                , if duration.minutes > 0 then Just (pluralize duration.minutes "minute") else Nothing
                , if duration.seconds > 0 then Just (pluralize duration.seconds "second") else Nothing
                ]

        in
        case parts of
            [] ->
                "just now"

            ps ->
                String.join " " ps ++ " ago"
