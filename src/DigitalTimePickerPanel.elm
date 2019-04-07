module DigitalTimePickerPanel exposing (Config, view)

import Date exposing (Date)
import DateTimePicker.Config exposing (CssConfig)
import DateTimePicker.DateUtils
import DateTimePicker.Events exposing (onBlurWithChange, onMouseDownPreventDefault, onMouseUpPreventDefault, onTouchEndPreventDefault, onTouchStartPreventDefault)
import DateTimePicker.Internal exposing (InternalState(..), Time)
import DateTimePicker.SharedStyles exposing (CssClasses(..))
import DateTimePicker.Svg
import Html exposing (Html, button, div, input, li, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes
import String


-- MODEL


{-| The state of the date time picker (for Internal Use)
-}
type alias State =
    InternalState


type alias Config otherConfig msg =
    { otherConfig
        | onChange : State -> Maybe Date -> msg
        , titleFormatter : Date -> String
    }



-- VIEWS


view : Bool -> Config (CssConfig a msg CssClasses) msg -> State -> Maybe Date.Date -> Html msg
view useMilitary config ((InternalState stateValue) as state) currentDate =
    if useMilitary then
        viewMilitary config state currentDate
    else
        viewNoMilitary config state currentDate


viewNoMilitary : Config (CssConfig a msg CssClasses) msg -> State -> Maybe Date.Date -> Html msg
viewNoMilitary config ((InternalState stateValue) as state) currentDate =
    let
        toListItem str =
            li [] [ text str ]

        hours =
            List.range stateValue.hourPickerStart (stateValue.hourPickerStart + 6)

        minutes =
            List.range stateValue.minutePickerStart (stateValue.minutePickerStart + 6)

        ampmList =
            [ "AM", "PM" ]

        timeSelector =
            List.map3 toRow hours minutes (ampmList ++ List.repeat 4 "")

        toRow hour min ampm =
            tr []
                [ hourCell hour
                , minuteCell min
                , amPmCell ampm
                ]

        hourCell hour =
            td
                [ onMouseDownPreventDefault <| hourClickHandler config state hour
                , onTouchStartPreventDefault <| hourClickHandler config state hour
                , stateValue.time.hour
                    |> Maybe.map ((==) hour)
                    |> Maybe.map
                        (\selected ->
                            if selected then
                                config.class [ SelectedHour ]
                            else
                                config.class []
                        )
                    |> Maybe.withDefault (config.class [])
                , Html.Attributes.attribute "role" "button"
                , Html.Attributes.attribute "aria-label" ("hour " ++ toString hour)
                ]
                [ text <| (toString >> DateTimePicker.DateUtils.padding) hour ]

        minuteCell min =
            td
                [ onMouseDownPreventDefault <| minuteClickHandler config state min
                , onTouchStartPreventDefault <| minuteClickHandler config state min
                , stateValue.time.minute
                    |> Maybe.map ((==) min)
                    |> Maybe.map
                        (\selected ->
                            if selected then
                                config.class [ SelectedMinute ]
                            else
                                config.class []
                        )
                    |> Maybe.withDefault (config.class [])
                , Html.Attributes.attribute "role" "button"
                , Html.Attributes.attribute "aria-label" ("minute " ++ toString min)
                ]
                [ text <| (toString >> DateTimePicker.DateUtils.padding) min ]

        amPmCell ampm =
            let
                defaultClasses =
                    config.class <|
                        if ampm == "" then
                            [ EmptyCell ]
                        else
                            []
            in
                td
                    ([ stateValue.time.amPm
                        |> Maybe.map ((==) ampm)
                        |> Maybe.map
                            (\selected ->
                                if selected then
                                    config.class [ SelectedAmPm ]
                                else
                                    defaultClasses
                            )
                        |> Maybe.withDefault defaultClasses
                     , Html.Attributes.attribute "role" "button"
                     , Html.Attributes.attribute "aria-label" ampm
                     ]
                        ++ (if ampm == "" then
                                []
                            else
                                [ onMouseDownPreventDefault <| amPmClickHandler config state ampm
                                , onTouchStartPreventDefault <| amPmClickHandler config state ampm
                                ]
                           )
                    )
                    [ text ampm ]

        upArrows config =
            [ tr [ config.class [ ArrowUp ] ]
                [ td
                    [ onMouseDownPreventDefault <| hourUpHandler config state currentDate
                    , onTouchStartPreventDefault <| hourUpHandler config state currentDate
                    ]
                    [ DateTimePicker.Svg.upArrow ]
                , td
                    [ onMouseDownPreventDefault <| minuteUpHandler config state currentDate
                    , onTouchStartPreventDefault <| minuteUpHandler config state currentDate
                    ]
                    [ DateTimePicker.Svg.upArrow ]
                , td [] []
                ]
            ]

        downArrows config =
            [ tr [ config.class [ ArrowDown ] ]
                [ td
                    [ onMouseDownPreventDefault <| hourDownHandler config state currentDate
                    , onTouchStartPreventDefault <| hourDownHandler config state currentDate
                    ]
                    [ DateTimePicker.Svg.downArrow ]
                , td
                    [ onMouseDownPreventDefault <| minuteDownHandler config state currentDate
                    , onTouchStartPreventDefault <| minuteDownHandler config state currentDate
                    ]
                    [ DateTimePicker.Svg.downArrow ]
                , td [] []
                ]
            ]
    in
        div [ config.class [ TimePickerDialog, DigitalTime ] ]
            [ div [ config.class [ Header ] ]
                [ Maybe.map config.titleFormatter currentDate |> Maybe.withDefault "-- : --" |> text ]
            , div [ config.class [ Body ] ]
                [ table []
                    [ tbody []
                        (upArrows config
                            ++ timeSelector
                            ++ downArrows config
                        )
                    ]
                ]
            ]


hourClickHandler : Config a msg -> State -> Int -> msg
hourClickHandler config (InternalState state) hour =
    let
        time =
            state.time

        updatedStateValue =
            { state | time = { time | hour = Just hour }, event = "hourClickHandler" }

        ( updatedTime, forceClose ) =
            case ( updatedStateValue.time.minute, updatedStateValue.time.amPm ) of
                ( Just minute, Just amPm ) ->
                    ( Just <| DateTimePicker.DateUtils.toTime hour minute amPm
                    , True
                    )

                _ ->
                    ( Nothing, False )
    in
        config.onChange (InternalState { updatedStateValue | forceClose = forceClose }) updatedTime


minuteClickHandler : Config a msg -> State -> Int -> msg
minuteClickHandler config (InternalState state) minute =
    let
        time =
            state.time

        updatedStateValue =
            { state | time = { time | minute = Just minute }, event = "minuteClickHandler" }

        ( updatedTime, forceClose ) =
            case ( updatedStateValue.time.hour, updatedStateValue.time.amPm ) of
                ( Just hour, Just amPm ) ->
                    ( Just <| DateTimePicker.DateUtils.toTime hour minute amPm
                    , True
                    )

                _ ->
                    ( Nothing, False )
    in
        config.onChange (InternalState { updatedStateValue | forceClose = forceClose }) updatedTime


amPmClickHandler : Config a msg -> State -> String -> msg
amPmClickHandler config (InternalState state) amPm =
    let
        time =
            state.time

        updatedStateValue =
            { state
                | time =
                    { time
                        | amPm =
                            if String.isEmpty amPm then
                                Nothing
                            else
                                Just amPm
                    }
                , event = "amPmClickHandler"
            }

        ( updatedTime, forceClose ) =
            case ( updatedStateValue.time.hour, updatedStateValue.time.minute ) of
                ( Just hour, Just minute ) ->
                    ( Just <| DateTimePicker.DateUtils.toTime hour minute amPm
                    , True
                    )

                _ ->
                    ( Nothing, False )
    in
        config.onChange (InternalState { updatedStateValue | forceClose = forceClose }) updatedTime


hourUpHandler : Config a msg -> State -> Maybe Date.Date -> msg
hourUpHandler config (InternalState state) currentDate =
    let
        updatedState =
            if state.hourPickerStart - 6 >= 1 then
                { state | hourPickerStart = state.hourPickerStart - 6 }
            else
                state
    in
        config.onChange (InternalState updatedState) currentDate


hourDownHandler : Config a msg -> State -> Maybe Date.Date -> msg
hourDownHandler config (InternalState state) currentDate =
    let
        updatedState =
            if state.hourPickerStart + 6 <= 12 then
                { state | hourPickerStart = state.hourPickerStart + 6 }
            else
                state
    in
        config.onChange (InternalState updatedState) currentDate


minuteUpHandler : Config a msg -> State -> Maybe Date.Date -> msg
minuteUpHandler config (InternalState state) currentDate =
    let
        updatedState =
            if state.minutePickerStart - 6 >= 0 then
                { state | minutePickerStart = state.minutePickerStart - 6 }
            else
                state
    in
        config.onChange (InternalState updatedState) currentDate


minuteDownHandler : Config a msg -> State -> Maybe Date.Date -> msg
minuteDownHandler config (InternalState state) currentDate =
    let
        updatedState =
            if state.minutePickerStart + 6 <= 59 then
                { state | minutePickerStart = state.minutePickerStart + 6 }
            else
                state
    in
        config.onChange (InternalState updatedState) currentDate


viewMilitary : Config (CssConfig a msg CssClasses) msg -> State -> Maybe Date.Date -> Html msg
viewMilitary config ((InternalState stateValue) as state) currentDate =
    let
        toListItem str =
            li [] [ text str ]

        hours =
            List.range stateValue.hourPickerStart (stateValue.hourPickerStart + 5)

        minutes =
            List.range stateValue.minutePickerStart (stateValue.minutePickerStart + 5)

        timeSelector =
            List.map2 toRow hours minutes

        toRow hour min =
            tr []
                [ hourCell hour
                , minuteCell min
                ]

        hourCell hour =
            td
                [ onMouseDownPreventDefault <| hourClickHandlerMilitary config state hour
                , onTouchStartPreventDefault <| hourClickHandlerMilitary config state hour
                , stateValue.time.hour
                    |> Maybe.map ((==) hour)
                    |> Maybe.map
                        (\selected ->
                            if selected then
                                config.class [ SelectedHour ]
                            else
                                config.class []
                        )
                    |> Maybe.withDefault (config.class [])
                , Html.Attributes.attribute "role" "button"
                , Html.Attributes.attribute "aria-label" ("hour " ++ toString hour)
                ]
                [ text <| (toString >> DateTimePicker.DateUtils.padding) hour ]

        minuteCell min =
            td
                [ onMouseDownPreventDefault <| minuteClickHandlerMilitary config state min
                , onTouchStartPreventDefault <| minuteClickHandlerMilitary config state min
                , stateValue.time.minute
                    |> Maybe.map ((==) min)
                    |> Maybe.map
                        (\selected ->
                            if selected then
                                config.class [ SelectedMinute ]
                            else
                                config.class []
                        )
                    |> Maybe.withDefault (config.class [])
                , Html.Attributes.attribute "role" "button"
                , Html.Attributes.attribute "aria-label" ("minute " ++ toString min)
                ]
                [ text <| (toString >> DateTimePicker.DateUtils.padding) min ]

        upArrows config =
            [ tr [ config.class [ ArrowUp ] ]
                [ td
                    [ onMouseDownPreventDefault <| hourUpHandlerMilitary config state currentDate
                    , onTouchStartPreventDefault <| hourUpHandlerMilitary config state currentDate
                    ]
                    [ DateTimePicker.Svg.upArrow ]
                , td
                    [ onMouseDownPreventDefault <| minuteUpHandlerMilitary config state currentDate
                    , onTouchStartPreventDefault <| minuteUpHandlerMilitary config state currentDate
                    ]
                    [ DateTimePicker.Svg.upArrow ]
                ]
            ]

        downArrows config =
            [ tr [ config.class [ ArrowDown ] ]
                [ td
                    [ onMouseDownPreventDefault <| hourDownHandlerMilitary config state currentDate
                    , onTouchStartPreventDefault <| hourDownHandlerMilitary config state currentDate
                    ]
                    [ DateTimePicker.Svg.downArrow ]
                , td
                    [ onMouseDownPreventDefault <| minuteDownHandlerMilitary config state currentDate
                    , onTouchStartPreventDefault <| minuteDownHandlerMilitary config state currentDate
                    ]
                    [ DateTimePicker.Svg.downArrow ]
                ]
            ]
    in
        div [ config.class [ TimePickerDialog, DigitalTime ] ]
            [ div [ config.class [ Header ] ]
                [ Maybe.map config.titleFormatter currentDate |> Maybe.withDefault "-- : --" |> text ]
            , div [ config.class [ Body ] ]
                [ table []
                    [ tbody []
                        (upArrows config
                            ++ timeSelector
                            ++ downArrows config
                        )
                    ]
                ]
            ]


hourClickHandlerMilitary : Config a msg -> State -> Int -> msg
hourClickHandlerMilitary config (InternalState state) hour =
    let
        time =
            state.time

        updatedStateValue =
            { state | time = { time | hour = Just hour }, event = "hourClickHandler" }

        ( updatedTime, forceClose ) =
            case updatedStateValue.time.minute of
                Just minute ->
                    ( Just <| DateTimePicker.DateUtils.toTimeMilitary hour minute
                    , True
                    )

                _ ->
                    ( Nothing, False )
    in
        config.onChange (InternalState { updatedStateValue | forceClose = forceClose }) updatedTime


minuteClickHandlerMilitary : Config a msg -> State -> Int -> msg
minuteClickHandlerMilitary config (InternalState state) minute =
    let
        time =
            state.time

        updatedStateValue =
            { state | time = { time | minute = Just minute }, event = "minuteClickHandler" }

        ( updatedTime, forceClose ) =
            case updatedStateValue.time.hour of
                Just hour ->
                    ( Just <| DateTimePicker.DateUtils.toTimeMilitary hour minute
                    , True
                    )

                _ ->
                    ( Nothing, False )
    in
        config.onChange (InternalState { updatedStateValue | forceClose = forceClose }) updatedTime


hourUpHandlerMilitary : Config a msg -> State -> Maybe Date.Date -> msg
hourUpHandlerMilitary config (InternalState state) currentDate =
    let
        updatedState =
            if state.hourPickerStart - 5 >= 0 then
                { state | hourPickerStart = state.hourPickerStart - 5 }
            else
                { state | hourPickerStart = 0 }
    in
        config.onChange (InternalState updatedState) currentDate


hourDownHandlerMilitary : Config a msg -> State -> Maybe Date.Date -> msg
hourDownHandlerMilitary config (InternalState state) currentDate =
    let
        updatedState =
            if state.hourPickerStart + 5 <= 23 then
                { state | hourPickerStart = state.hourPickerStart + 5 }
            else
                { state | hourPickerStart = 23 - 5 }
    in
        config.onChange (InternalState updatedState) currentDate


minuteUpHandlerMilitary : Config a msg -> State -> Maybe Date.Date -> msg
minuteUpHandlerMilitary config (InternalState state) currentDate =
    let
        updatedState =
            if state.minutePickerStart - 5 >= 0 then
                { state | minutePickerStart = state.minutePickerStart - 5 }
            else
                { state | minutePickerStart = 0 }
    in
        config.onChange (InternalState updatedState) currentDate


minuteDownHandlerMilitary : Config a msg -> State -> Maybe Date.Date -> msg
minuteDownHandlerMilitary config (InternalState state) currentDate =
    let
        updatedState =
            if state.minutePickerStart + 5 <= 59 then
                { state | minutePickerStart = state.minutePickerStart + 5 }
            else
                { state | minutePickerStart = 59 - 5 }
    in
        config.onChange (InternalState updatedState) currentDate
