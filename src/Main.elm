port module Main exposing (main)

import Browser
import Html exposing (Html, article, button, div, h1, h2, h3, input, li, nav, p, section, span, text, ul)
import Html.Attributes exposing (class, classList, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import String


port saveFoods : Encode.Value -> Cmd msg


port clearFoods : () -> Cmd msg


port foodsLoaded : (Decode.Value -> msg) -> Sub msg


port requestNow : () -> Cmd msg


port nowLoaded : (Int -> msg) -> Sub msg


type alias Model =
    { activeTab : Tab
    , overlay : Overlay
    , foods : List Food
    , nextFoodId : Int
    , draftName : String
    , draftEmoji : String
    , draftPrepStyle : PrepStyle
    , draftNote : String
    , storageReady : Bool
    , pendingLog : Maybe PendingLog
    , currentTime : Maybe Int
    }


type Tab
    = Tracker
    | History
    | Settings


type Overlay
    = NoOverlay
    | AddFood
    | Detail Int


type Tier
    = Active
    | Shelved
    | Mastered


type PrepStyle
    = Raw
    | Roasted
    | Steamed
    | Mashed
    | Sliced
    | Mixed
    | Dip
    | OtherPrep


type Interaction
    = Look
    | Touch
    | Smell
    | Taste
    | Eat


type Stage
    = Newbie
    | Learning
    | Growing
    | MasteredStage


type alias Food =
    { id : Int
    , name : String
    , emoji : String
    , category : String
    , tier : Tier
    , createdAt : Int
    , logs : List FoodLog
    }


type alias FoodLog =
    { at : Int
    , interaction : Interaction
    , prepStyle : PrepStyle
    , note : String
    }


type alias PendingLog =
    { foodId : Int
    , interaction : Interaction
    , prepStyle : PrepStyle
    , note : String
    }


type alias FoodChoice =
    { emoji : String
    , name : String
    }


defaultFoods : List Food
defaultFoods =
    [ seedFood 1 "Broccoli" "🥦" "Vegetable" Active []
    , seedFood 2 "Sweet Potato" "🍠" "Root vegetable" Active []
    , seedFood 3 "Banana" "🍌" "Fruit" Active []
    , seedFood 4 "Avocado" "🥑" "Healthy fat" Shelved []
    ]


seedFood : Int -> String -> String -> String -> Tier -> List FoodLog -> Food
seedFood id name emoji category tier logs =
    { id = id
    , name = name
    , emoji = emoji
    , category = category
    , tier = tier
    , createdAt = 0
    , logs = logs
    }


foodChoices : List FoodChoice
foodChoices =
    [ { emoji = "🍎", name = "Apple" }
    , { emoji = "🍌", name = "Banana" }
    , { emoji = "🍓", name = "Strawberry" }
    , { emoji = "🫐", name = "Blueberries" }
    , { emoji = "🍐", name = "Pear" }
    , { emoji = "🍊", name = "Orange" }
    , { emoji = "🍉", name = "Watermelon" }
    , { emoji = "🥦", name = "Broccoli" }
    , { emoji = "🥕", name = "Carrot" }
    , { emoji = "🍠", name = "Sweet Potato" }
    , { emoji = "🌽", name = "Corn" }
    , { emoji = "🍝", name = "Pasta" }
    , { emoji = "🍚", name = "Rice" }
    , { emoji = "🍞", name = "Bread" }
    , { emoji = "🥣", name = "Oatmeal" }
    , { emoji = "🧀", name = "Cheese" }
    , { emoji = "🥛", name = "Milk" }
    , { emoji = "🥚", name = "Egg" }
    , { emoji = "🍗", name = "Chicken" }
    , { emoji = "🐟", name = "Fish" }
    , { emoji = "🥑", name = "Avocado" }
    , { emoji = "🍪", name = "Snack" }
    ]


prepStyles : List PrepStyle
prepStyles =
    [ Raw, Roasted, Steamed, Mashed, Sliced, Mixed, Dip, OtherPrep ]


initialModel : Model
initialModel =
    { activeTab = Tracker
    , overlay = NoOverlay
    , foods = defaultFoods
    , nextFoodId = nextFoodId defaultFoods
    , draftName = ""
    , draftEmoji = "🍎"
    , draftPrepStyle = Raw
    , draftNote = ""
    , storageReady = False
    , pendingLog = Nothing
    , currentTime = Nothing
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, requestNow () )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = SelectTab Tab
    | OpenAddFood
    | OpenDetail Int
    | CloseOverlay
    | UpdateDraftName String
    | UpdateDraftEmoji String
    | UpdateDraftPrepStyle PrepStyle
    | UpdateDraftNote String
    | CreateFood
    | StartLog Int Interaction
    | ToggleShelf Int
    | DeleteFood Int
    | ResetFoods
    | ReceiveFoods Decode.Value
    | ReceiveNow Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ foodsLoaded ReceiveFoods
        , nowLoaded ReceiveNow
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab tab ->
            ( { model | activeTab = tab, overlay = NoOverlay }, Cmd.none )

        OpenAddFood ->
            ( { model | overlay = AddFood, draftName = "", draftEmoji = "🍎", draftPrepStyle = Raw, draftNote = "" }, Cmd.none )

        OpenDetail foodId ->
            ( { model | overlay = Detail foodId }, Cmd.none )

        CloseOverlay ->
            ( { model | overlay = NoOverlay, pendingLog = Nothing }, Cmd.none )

        UpdateDraftName draftName ->
            ( { model | draftName = draftName }, Cmd.none )

        UpdateDraftEmoji draftEmoji ->
            ( { model
                | draftEmoji = draftEmoji
                , draftName =
                    if String.trim model.draftName == "" then
                        defaultFoodNameForEmoji draftEmoji

                    else
                        model.draftName
              }
            , Cmd.none
            )

        UpdateDraftPrepStyle draftPrepStyle ->
            ( { model | draftPrepStyle = draftPrepStyle }, Cmd.none )

        UpdateDraftNote draftNote ->
            ( { model | draftNote = draftNote }, Cmd.none )

        CreateFood ->
            let
                name =
                    String.trim model.draftName

                emoji =
                    String.trim model.draftEmoji

                nextFood =
                    { id = model.nextFoodId
                    , name = name
                    , emoji = if emoji == "" then "🍎" else emoji
                    , category = "Custom"
                    , tier = Active
                    , createdAt = 0
                    , logs = []
                    }
            in
            if name == "" then
                ( model, Cmd.none )

            else
                persist
                    { model
                        | foods = model.foods ++ [ nextFood ]
                        , nextFoodId = model.nextFoodId + 1
                        , draftName = ""
                        , draftEmoji = "🍎"
                        , draftPrepStyle = Raw
                        , draftNote = ""
                        , overlay = NoOverlay
                    }

        StartLog foodId interaction ->
            if model.pendingLog /= Nothing then
                ( model, Cmd.none )

            else
                ( { model
                    | pendingLog =
                        Just
                            { foodId = foodId
                            , interaction = interaction
                            , prepStyle = model.draftPrepStyle
                            , note = String.trim model.draftNote
                            }
                  }
                , requestNow ()
                )

        ToggleShelf foodId ->
            let
                updatedFoods =
                    updateFoodById
                        foodId
                        (\food ->
                            { food
                                | tier =
                                    case food.tier of
                                        Shelved ->
                                            Active

                                        _ ->
                                            Shelved
                            }
                        )
                        model.foods
            in
            persist { model | foods = updatedFoods }

        DeleteFood foodId ->
            let
                remainingFoods =
                    List.filter (\food -> food.id /= foodId) model.foods

                overlay =
                    case model.overlay of
                        Detail currentId ->
                            if currentId == foodId then
                                NoOverlay

                            else
                                model.overlay

                        _ ->
                            model.overlay
            in
            persist
                { model
                    | foods = remainingFoods
                    , nextFoodId = nextFoodId remainingFoods
                    , overlay = overlay
                }

        ResetFoods ->
            resetAllFoods model

        ReceiveFoods raw ->
            case Decode.decodeValue (Decode.nullable (Decode.list foodDecoder)) raw of
                Ok maybeFoods ->
                    let
                        loadedFoods =
                            Maybe.withDefault model.foods maybeFoods
                    in
                    ( { model
                        | foods = loadedFoods
                        , nextFoodId = nextFoodId loadedFoods
                        , storageReady = True
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | storageReady = True }, Cmd.none )

        ReceiveNow now ->
            case model.pendingLog of
                Nothing ->
                    ( { model | currentTime = Just now }, Cmd.none )

                Just pending ->
                    let
                        updatedFoods =
                            applyPendingLog now pending model.foods
                    in
                    persist
                        { model
                            | foods = updatedFoods
                            , pendingLog = Nothing
                            , overlay = NoOverlay
                            , currentTime = Just now
                        }


persist : Model -> ( Model, Cmd Msg )
persist model =
    if model.storageReady then
        ( model, saveFoods (Encode.list foodEncoder model.foods) )

    else
        ( model, Cmd.none )


resetAllFoods : Model -> ( Model, Cmd Msg )
resetAllFoods model =
    ( { model
        | activeTab = Tracker
        , overlay = NoOverlay
        , foods = []
        , nextFoodId = 1
        , draftName = ""
        , draftEmoji = "🍎"
        , draftPrepStyle = Raw
        , draftNote = ""
        , pendingLog = Nothing
      }
    , clearFoods ()
    )


view : Model -> Html Msg
view model =
    div
        [ class "relative min-h-screen overflow-hidden text-slate-800"
        , style "background" "radial-gradient(circle at top, rgba(106, 147, 37, 0.15), transparent 34%), linear-gradient(180deg, #f8f6ef 0%, #f3f1e6 100%)"
        ]
        [ div
            [ class "mx-auto flex min-h-screen w-full max-w-[440px] flex-col px-4 pb-28 pt-4" ]
            [ headerView model
            , mainView model
            , bottomNav model
            , floatingAction
            ]
        , overlayView model
        ]


headerView : Model -> Html Msg
headerView _ =
    section
        [ class "flex items-center justify-between pb-5" ]
        [ div [ class "flex items-center gap-3" ]
            [ avatar
            , div []
                [ h1 [ class "text-[28px] font-extrabold tracking-tight text-[#446b0a]" ]
                    [ text "Toddler Tracker" ]
                , p [ class "mt-1 text-[12px] font-semibold uppercase tracking-[0.28em] text-slate-500" ]
                    [ text "calm mealtime logging" ]
                ]
            ]
        , button
            [ class "grid h-12 w-12 place-items-center rounded-full bg-[#dfe8d1] text-3xl font-light text-[#5a8c10] shadow-sm transition hover:scale-105"
            , onClick OpenAddFood
            ]
            [ text "+" ]
        ]


mainView : Model -> Html Msg
mainView model =
    case model.activeTab of
        Tracker ->
            trackerView model

        History ->
            historyView model

        Settings ->
            settingsView


trackerView : Model -> Html Msg
trackerView model =
    let
        activeFoods =
            sortActiveFoods model.foods

        shelvedFoods =
            List.filter (\food -> food.tier == Shelved) model.foods

        masteredFoods =
            List.filter (\food -> food.tier == Mastered) model.foods
    in
    div [ class "flex flex-1 flex-col gap-8" ]
        [ heroBanner activeFoods masteredFoods
        , section [ class "space-y-4" ]
            [ sectionHeading "Active Queue" "Surfacing foods not seen recently"
            , if List.isEmpty activeFoods then
                emptyState "No active foods yet" "Add a food and start logging what they look at, touch, taste, or eat."

              else
                div [ class "space-y-4" ] (List.map (activeFoodCard model) activeFoods)
            ]
        , section [ class "space-y-4" ]
            [ sectionHeading "Shelved" "Paused foods for a calmer season"
            , shelvedFoodsView shelvedFoods
            ]
        , section [ class "space-y-4" ]
            [ sectionHeading "Mastered" "Foods that are eaten reliably"
            , masteredFoodsView model masteredFoods
            ]
        ]


heroBanner : List Food -> List Food -> Html Msg
heroBanner activeFoods masteredFoods =
    article
        [ class "rounded-[36px] bg-[linear-gradient(135deg,#f1ffe0_0%,#f8fbf0_48%,#ffffff_100%)] px-6 py-7 shadow-[0_18px_36px_rgba(91,122,28,0.10)] ring-1 ring-white/80" ]
        [ p [ class "text-[12px] uppercase tracking-[0.35em] text-[#6a8b26]" ]
            [ text "Ready for a bite?" ]
        , h2 [ class "mt-3 text-[34px] font-extrabold leading-tight tracking-tight text-slate-800" ]
            [ text "Log mealtime in two taps." ]
        , p [ class "mt-3 max-w-[290px] text-[17px] leading-7 text-slate-600" ]
            [ text "Choose a prep style once, then log look, touch, taste, or eat without extra friction." ]
        , div [ class "mt-5 flex flex-wrap gap-3" ]
            [ statPill (String.fromInt (List.length activeFoods) ++ " active")
            , statPill (String.fromInt (List.length masteredFoods) ++ " mastered")
            ]
        ]


statPill : String -> Html Msg
statPill label =
    span
        [ class "rounded-full bg-white px-4 py-2 text-sm font-bold text-[#476b0d] shadow-sm ring-1 ring-[#dbe7c5]" ]
        [ text label ]


sectionHeading : String -> String -> Html Msg
sectionHeading title subtitle =
    div []
        [ h2 [ class "text-[28px] font-extrabold tracking-tight text-slate-800" ] [ text title ]
        , p [ class "mt-1 text-[16px] leading-6 text-slate-500" ] [ text subtitle ]
        ]


activeFoodCard : Model -> Food -> Html Msg
activeFoodCard model food =
    let
        now =
            currentNow model

        counts =
            interactionCounts food.logs

        summary =
            foodSummary food

        ageLabel =
            recencyLabel now (foodLastLoggedAt food)

        maintenanceDue =
            needsMaintenance now food

        phaseLabel =
            if masteryStreak food.logs >= 3 then
                "EAT PHASE"

            else if counts.eatCount > 0 then
                "EAT PHASE"

            else if counts.tasteCount > 0 then
                "TASTE PHASE"

            else if counts.touchCount > 0 || counts.smellCount > 0 then
                "TOUCH PHASE"

            else
                "LOOK PHASE"

        middleCount =
            counts.touchCount + counts.smellCount + counts.tasteCount
    in
    article
        [ classList
            [ ( "rounded-[34px] bg-[linear-gradient(180deg,#fbfcf6_0%,#f5f7ee_100%)] p-4 shadow-[0_12px_28px_rgba(116,124,87,0.08)] ring-1 ring-white/90", True )
            , ( "ring-[#dfe8ce]", food.tier == Active )
            ]
        ]
        [ div [ class "flex items-start justify-between gap-3" ]
            [ div [ class "flex items-center gap-3" ]
                [ foodIcon food
                , div [ class "min-w-0" ]
                    [ h3 [ class "text-[28px] font-extrabold leading-none tracking-tight text-slate-800" ] [ text food.name ]
                    , p [ class "mt-1.5 text-[13px] font-bold uppercase tracking-[0.24em] text-[#60718f]" ] [ text food.category ]
                    ]
                ]
            , div [ class "flex flex-col items-end gap-1.5" ]
                [ tierBadge food.tier
                , span
                    [ classList
                        [ ( "rounded-full px-3.5 py-1.5 text-[11px] font-extrabold uppercase tracking-[0.22em]", True )
                        , ( "bg-[#f5dccf] text-[#b43700]", maintenanceDue )
                        , ( "bg-[#e8f3d0] text-[#4b7a00]", not maintenanceDue )
                        ]
                    ]
                    [ text ageLabel ]
                ]
            ]
        , div [ class "mt-5" ]
            [ div [ class "flex items-start justify-between gap-3" ]
                [ p [ class "text-[14px] font-extrabold uppercase tracking-[0.32em] text-[#5d6f8c]" ] [ text "Habituation" ]
                , p [ class "text-[15px] font-extrabold uppercase tracking-[0.24em] text-[#4b7a00]" ] [ text phaseLabel ]
                ]
            , brokenProgressBar counts.lookCount middleCount counts.eatCount
            ]
        , p [ class "mt-4 text-[14px] leading-6 text-slate-600" ]
            [ text summary ]
        , masterySequence food.logs
        , div [ class "mt-5" ]
            [ button
                [ class "w-full rounded-full bg-white px-4 py-3.5 text-[24px] font-extrabold text-[#487600] shadow-[0_14px_26px_rgba(56,95,0,0.10)] ring-1 ring-white/80"
                , onClick (OpenDetail food.id)
                ]
                [ text "Log Try" ]
            ]
        ]


brokenProgressBar : Int -> Int -> Int -> Html Msg
brokenProgressBar lookCount middleCount eatCount =
    let
        lookWidth =
            progressWidth lookCount

        middleWidth =
            progressWidth middleCount

        eatWidth =
            progressWidth eatCount
    in
    div [ class "mt-2" ]
        [ div [ class "grid grid-cols-3 gap-2" ]
            [ progressSegment lookWidth False
            , progressSegment middleWidth False
            , progressSegment eatWidth True
            ]
        , div [ class "mt-1.5 grid grid-cols-3 gap-2" ]
            [ progressLabel "Look"
            , progressLabel "Touch"
            , progressLabel "Taste"
            ]
        ]


progressSegment : String -> Bool -> Html Msg
progressSegment width isEat =
    div
        [ class "relative h-4 overflow-hidden rounded-full bg-[#e6ecd9]" ]
        [ div
            [ classList
                [ ( "absolute inset-y-0 left-0 rounded-full", True )
                , ( "bg-[linear-gradient(90deg,#355f00_0%,#4e8500_55%,#6fa300_100%)]", not isEat )
                , ( "bg-[linear-gradient(90deg,#c8ee67_0%,#a6de3c_100%)]", isEat )
                ]
            , style "width" width
            ]
            []
        ]


progressLabel : String -> Html Msg
progressLabel label =
    p [ class "text-center text-[14px] font-medium text-[#90948c]" ]
        [ text label ]


progressWidth : Int -> String
progressWidth count =
    if count <= 0 then
        "0%"

    else
        let
            width =
                clamp 0.08 1 (toFloat count / 3)
        in
        String.fromFloat (width * 100) ++ "%"


masterySequence : List FoodLog -> Html Msg
masterySequence logs =
    let
        streak =
            masteryStreak logs
    in
    div
        [ class "mt-5 rounded-[30px] bg-white/82 p-3.5 shadow-[0_10px_18px_rgba(118,129,94,0.06)] ring-1 ring-white/80" ]
        [ div [ class "flex items-center gap-4" ]
            [ span [ class "text-[20px] leading-none text-[#a36a00]" ] [ text "★" ]
            , h3 [ class "text-[16px] font-extrabold tracking-tight text-slate-700" ] [ text "Mastery Sequence" ]
            , div [ class "ml-auto flex items-center gap-1.5" ]
                (List.map (masteryDot streak) [ 1, 2, 3 ])
            ]
        ]


masteryDot : Int -> Int -> Html Msg
masteryDot streak step =
    let
        active =
            streak >= step
    in
    span
        [ classList
            [ ( "grid h-9 w-9 place-items-center rounded-full text-[16px] font-black transition", True )
            , ( "bg-[#ffcb8f] text-[#8b4c00]", active )
            , ( "bg-[#e0e0da] text-transparent", not active )
            ]
        ]
        [ text "•" ]


shelvedFoodsView : List Food -> Html Msg
shelvedFoodsView foods =
    if List.isEmpty foods then
        emptyState "Nothing is shelved" "Pause foods here when you need less mealtime pressure."

    else
        div [ class "grid grid-cols-2 gap-4" ]
            (List.map shelvedCard foods)


shelvedCard : Food -> Html Msg
shelvedCard food =
    article
        [ class "rounded-[28px] bg-white/86 p-4 text-center shadow-[0_8px_24px_rgba(86,86,44,0.06)] ring-1 ring-white/80" ]
        [ div [ class "emoji mx-auto grid h-16 w-16 place-items-center rounded-full bg-[#efe9db] text-[34px] shadow-inner" ]
            [ text food.emoji ]
        , h3 [ class "mt-3 text-[19px] font-extrabold text-slate-800" ] [ text food.name ]
        , p [ class "mt-1 text-xs font-bold uppercase tracking-[0.28em] text-slate-500" ] [ text "Shelved" ]
        , button
            [ class "mt-4 rounded-full border border-[#d8dfc7] bg-white px-4 py-2 text-sm font-bold text-slate-700"
            , onClick (ToggleShelf food.id)
            ]
            [ text "Return" ]
        ]


masteredFoodsView : Model -> List Food -> Html Msg
masteredFoodsView model foods =
    if List.isEmpty foods then
        emptyState "No mastered foods yet" "Once a food is eaten across consecutive offerings, it moves here."

    else
        div [ class "grid grid-cols-2 gap-4" ]
            (List.map (masteredCard model) foods)


masteredCard : Model -> Food -> Html Msg
masteredCard model food =
    let
        stale =
            needsMaintenance (currentNow model) food
    in
    article
        [ classList
            [ ( "relative rounded-[28px] bg-white p-4 text-center shadow-[0_8px_24px_rgba(86,86,44,0.06)] ring-1", True )
            , ( "ring-[#e7d7b8]", stale )
            , ( "ring-[#dfe8d1]", not stale )
            ]
        ]
        [ div
            [ classList
                [ ( "emoji absolute -right-1 -top-1 grid h-8 w-8 place-items-center rounded-full text-sm font-bold text-white shadow-md", True )
                , ( "bg-[#c84d24]", stale )
                , ( "bg-[#487800]", not stale )
                ]
            ]
            [ text (if stale then "!" else "✓") ]
        , div [ class "emoji mx-auto grid h-16 w-16 place-items-center rounded-full bg-[#f2efdc] text-[34px] shadow-inner" ]
            [ text food.emoji ]
        , h3 [ class "mt-3 text-[19px] font-extrabold text-slate-800" ] [ text food.name ]
        , p [ classList [ ( "mt-1 text-xs font-bold uppercase tracking-[0.28em]", True ), ( "text-[#c84d24]", stale ), ( "text-slate-500", not stale ) ] ]
            [ text (if stale then "Maintenance due" else "Stable") ]
        , p [ class "mt-1 text-sm text-slate-600" ]
            [ text (maintenanceCopy stale) ]
        , button
            [ class "mt-4 rounded-full border border-[#d8dfc7] bg-white px-4 py-2 text-sm font-bold text-slate-700"
            , onClick (OpenDetail food.id)
            ]
            [ text "Check in" ]
        ]


emptyState : String -> String -> Html Msg
emptyState title body =
    article
        [ class "rounded-[30px] bg-white/82 p-6 text-center shadow-[0_8px_30px_rgba(120,120,80,0.08)] ring-1 ring-white/70" ]
        [ h3 [ class "text-[22px] font-extrabold text-slate-800" ] [ text title ]
        , p [ class "mt-2 text-[16px] leading-7 text-slate-500" ] [ text body ]
        ]


historyView : Model -> Html Msg
historyView model =
    let
        items =
            recentLogItems model.foods

        now =
            currentNow model
    in
    div [ class "flex flex-1 flex-col gap-6" ]
        [ sectionHeading "History" "A quiet record of progress and refusals"
        , if List.isEmpty items then
            emptyState "No logs yet" "Use the tracker tab to start building the timeline."

          else
            div [ class "space-y-4" ] (List.map (historyCard now) items)
        ]


historyCard :
    Int
    ->
    { at : Int
    , food : String
    , interaction : Interaction
    , prepStyle : PrepStyle
    , note : String
    }
    -> Html Msg
historyCard now item =
    article
        [ class "rounded-[28px] bg-white/88 p-5 shadow-[0_8px_26px_rgba(120,120,80,0.08)] ring-1 ring-white/70" ]
        [ p [ class "text-xs font-bold uppercase tracking-[0.28em] text-slate-500" ] [ text (recencyLabel now (Just item.at)) ]
        , h3 [ class "mt-2 text-[22px] font-extrabold text-slate-800" ] [ text item.food ]
        , div [ class "mt-3 flex flex-wrap items-center gap-2" ]
            [ span [ class "rounded-full bg-lime-200 px-3 py-1 text-sm font-semibold text-lime-950" ] [ text (interactionLabel item.interaction) ]
            , span [ class "rounded-full bg-slate-100 px-3 py-1 text-sm font-semibold text-slate-700" ] [ text (prepStyleLabel item.prepStyle) ]
            , if String.trim item.note == "" then
                text ""

              else
                span [ class "text-sm text-slate-500" ] [ text item.note ]
            ]
        ]


settingsView : Html Msg
settingsView =
    div [ class "flex flex-1 flex-col gap-6 pt-2 pb-6" ]
        [ article
            [ class "rounded-[40px] bg-white px-6 py-8 shadow-[0_18px_42px_rgba(130,120,90,0.12)] ring-1 ring-white/70" ]
            [ h2 [ class "text-[24px] font-extrabold tracking-tight text-[#1f2d4a]" ] [ text "Reset System" ]
            , p [ class "mt-4 text-[17px] leading-[1.75] text-[#4b5d7f]" ]
                [ text "Clear the tracker and start over with a fresh food routine." ]
            , button
                [ class "mt-8 rounded-full bg-[linear-gradient(180deg,#c6ed8a_0%,#b8e57a_100%)] px-6 py-4 text-[14px] font-extrabold tracking-[0.32em] text-[#4c8a00] shadow-[0_10px_18px_rgba(123,173,40,0.18)]"
                , onClick ResetFoods
                ]
                [ text "RESET ALL DATA" ]
            ]
        , article
            [ class "rounded-[40px] bg-[#f7e6dc] px-6 py-8 shadow-[0_18px_42px_rgba(194,142,111,0.12)] ring-1 ring-white/40" ]
            [ h2 [ class "text-[24px] font-extrabold tracking-tight text-[#1f2d4a]" ] [ text "About the tracker" ]
            , p [ class "mt-4 text-[17px] leading-[1.75] text-[#4b5d7f]" ]
                [ text "Mastery is awarded only after consecutive eating logs. Look, touch, smell, and taste still count as progress, but they stay in the learning lane." ]
            ]
        ]


floatingAction : Html Msg
floatingAction =
    button
        [ class "fixed bottom-28 right-6 z-20 grid h-16 w-16 place-items-center rounded-full bg-[#3d7800] text-[32px] font-light text-white shadow-[0_18px_36px_rgba(61,120,0,0.35)] transition hover:scale-105"
        , onClick OpenAddFood
        ]
        [ text "+" ]


bottomNav : Model -> Html Msg
bottomNav model =
    nav
        [ class "fixed inset-x-0 bottom-0 z-20 px-4 pb-4" ]
        [ div
            [ class "mx-auto flex max-w-[440px] items-center justify-around rounded-[30px] bg-[linear-gradient(180deg,#edf1e6_0%,#e1e7d7_100%)] px-4 py-3 shadow-[0_10px_32px_rgba(111,111,78,0.12)]" ]
            [ tabButton Tracker model.activeTab "Tracker" "🍽️"
            , tabButton History model.activeTab "History" "🕘"
            , tabButton Settings model.activeTab "Settings" "⚙️"
            ]
        ]


tabButton : Tab -> Tab -> String -> String -> Html Msg
tabButton tab activeTab label emoji =
    let
        isActive =
            tab == activeTab
    in
    button
        [ classList
            [ ( "grid place-items-center rounded-full px-4 py-3 transition", True )
            , ( "bg-[#3f7f09] text-white shadow-md", isActive )
            , ( "text-slate-700", not isActive )
            ]
        , onClick (SelectTab tab)
        ]
        [ div [ class "emoji text-[22px] leading-none" ] [ text emoji ]
        , div [ class "mt-1 text-[11px] font-bold uppercase tracking-[0.35em]" ] [ text label ]
        ]


overlayView : Model -> Html Msg
overlayView model =
    case model.overlay of
        NoOverlay ->
            text ""

        AddFood ->
            addFoodOverlay model

        Detail foodId ->
            case foodById foodId model.foods of
                Just food ->
                    detailOverlay model food

                Nothing ->
                    text ""


addFoodOverlay : Model -> Html Msg
addFoodOverlay model =
    div
        [ class "fixed inset-0 z-30 bg-black/25 px-3 py-8 backdrop-blur-sm" ]
        [ div
            [ class "mx-auto flex h-full max-w-[420px] items-end" ]
            [ article
                [ class "w-full rounded-[36px] bg-[#f8f7ef] p-6 shadow-[0_24px_60px_rgba(72,72,52,0.25)]" ]
                [ div [ class "flex items-start justify-between gap-4" ]
                    [ h2 [ class "text-[32px] font-extrabold tracking-tight text-slate-800" ] [ text "New Food" ]
                    , button
                        [ class "grid h-11 w-11 place-items-center rounded-full bg-[#dfe4d8] text-2xl text-slate-700"
                        , onClick CloseOverlay
                        ]
                        [ text "×" ]
                    ]
                , p [ class "mt-5 text-sm font-extrabold uppercase tracking-[0.25em] text-slate-500" ]
                    [ text "Pick an emoji" ]
                , emojiChoiceGrid model.draftEmoji foodChoices
                , p [ class "mt-7 text-sm font-extrabold uppercase tracking-[0.25em] text-slate-500" ]
                    [ text "Food name" ]
                , input
                    [ class "mt-3 w-full rounded-[30px] bg-[#edf1e2] px-6 py-5 text-xl font-semibold text-slate-700 outline-none placeholder:text-slate-400"
                    , placeholder "What's on the plate?"
                    , value model.draftName
                    , onInput UpdateDraftName
                    ]
                    []
                , button
                    [ class "mt-8 w-full rounded-full bg-[linear-gradient(180deg,#497f00_0%,#2f5d00_100%)] py-5 text-[22px] font-extrabold text-white shadow-[0_16px_28px_rgba(58,96,0,0.30)]"
                    , onClick CreateFood
                    ]
                    [ text "Add Food" ]
                ]
            ]
        ]


detailOverlay : Model -> Food -> Html Msg
detailOverlay model food =
    let
        lastInteractionText =
            case food.logs of
                latest :: _ ->
                    interactionLabel latest.interaction ++ " · " ++ prepStyleLabel latest.prepStyle

                [] ->
                    "No logs yet"
    in
    div
        [ class "fixed inset-0 z-30 overflow-y-auto bg-black/15 px-3 py-4 backdrop-blur-sm" ]
        [ div
            [ class "mx-auto flex min-h-full max-w-[420px] items-center py-2" ]
            [ article
                [ class "flex w-full max-h-[calc(100dvh-2rem)] flex-col overflow-y-auto rounded-[36px] bg-[#fbfbf8] p-5 shadow-[0_24px_60px_rgba(72,72,52,0.24)] sm:p-6" ]
                [ button
                    [ class "self-start text-left text-[22px] font-semibold text-slate-600"
                    , onClick CloseOverlay
                    ]
                    [ text "← Back" ]
                , div [ class "mt-5 grid place-items-center" ]
                    [ div [ class "emoji grid h-24 w-24 place-items-center rounded-full bg-[#eaf8d9] text-[60px] sm:h-28 sm:w-28 sm:text-[72px]" ]
                        [ text food.emoji ]
                    ]
                , h2 [ class "mt-5 text-center text-[30px] font-extrabold tracking-tight text-slate-800 sm:text-[34px]" ] [ text food.name ]
                , div [ class "mt-3 flex items-center justify-center gap-2" ]
                    [ tierBadge food.tier
                    , span [ class "rounded-full bg-[#ffcb8e] px-4 py-2 text-xs font-extrabold uppercase tracking-[0.25em] text-[#8a4d00] sm:text-sm" ]
                        [ text lastInteractionText ]
                    ]
                , p [ class "mt-4 text-center text-[16px] leading-7 text-slate-600 sm:text-[18px] sm:leading-8" ]
                    [ text "Pick a prep style once, then tap the interaction that happened today." ]
                , prepStylePicker model.draftPrepStyle
                , div [ class "mt-5 space-y-3" ]
                    [ interactionButton food.id model.draftPrepStyle model.draftNote Look "👀" "Look"
                    , interactionButton food.id model.draftPrepStyle model.draftNote Touch "✋" "Touch"
                    , interactionButton food.id model.draftPrepStyle model.draftNote Smell "👃" "Smell"
                    , interactionButton food.id model.draftPrepStyle model.draftNote Taste "👅" "Taste"
                    , interactionButton food.id model.draftPrepStyle model.draftNote Eat "😋" "Eat"
                    ]
                , notePills
                , input
                    [ class "mt-4 w-full rounded-[24px] bg-[#eef2e6] px-4 py-4 text-[16px] text-slate-700 outline-none placeholder:text-slate-400"
                    , placeholder "Optional note: texture, refusal, dip, mood..."
                    , value model.draftNote
                    , onInput UpdateDraftNote
                    ]
                    []
                , div [ class "mt-5 flex gap-3" ]
                    [ button
                        [ class "flex-1 rounded-full border-2 border-rose-300 bg-white py-3 text-[16px] font-extrabold text-rose-700 sm:py-4 sm:text-[18px]"
                        , onClick (DeleteFood food.id)
                        ]
                        [ text "Delete" ]
                    , button
                        [ class "flex-1 rounded-full border border-[#d8dfc7] bg-white py-3 text-[16px] font-extrabold text-slate-700 sm:py-4 sm:text-[18px]"
                        , onClick (ToggleShelf food.id)
                        ]
                        [ text (if food.tier == Shelved then "Return" else "Shelf") ]
                    ]
                , p [ class "mt-4 text-center text-sm leading-7 text-slate-500" ]
                    [ text "Look, touch, and taste build familiarity. Eating a food across consecutive offerings is what promotes it to mastered." ]
                ]
            ]
        ]


prepStylePicker : PrepStyle -> Html Msg
prepStylePicker selected =
    ul [ class "mt-6 grid list-none grid-cols-4 gap-3 p-0" ]
        (List.map (prepStyleChip selected) prepStyles)


prepStyleChip : PrepStyle -> PrepStyle -> Html Msg
prepStyleChip selected styleChoice =
    li
        [ classList
            [ ( "flex min-h-[64px] items-center justify-center rounded-2xl px-2 text-center transition", True )
            , ( "bg-[#eef1e7] text-slate-700", styleChoice /= selected )
            , ( "bg-[#d9efb8] text-[#436f00] ring-2 ring-[#7fb83a]", styleChoice == selected )
            ]
        , onClick (UpdateDraftPrepStyle styleChoice)
        ]
        [ span [ class "text-[13px] font-bold leading-tight" ] [ text (prepStyleLabel styleChoice) ] ]


notePills : Html Msg
notePills =
    div [ class "mt-4 flex flex-wrap gap-2" ]
        (List.map noteChip [ "Spit out", "Wanted dip", "Pushed away", "Loved it", "Too crunchy" ])


noteChip : String -> Html Msg
noteChip label =
    button
        [ class "rounded-full bg-white px-3 py-2 text-xs font-bold text-slate-600 ring-1 ring-[#dbe4cd]"
        , onClick (UpdateDraftNote label)
        ]
        [ text label ]


interactionButton : Int -> PrepStyle -> String -> Interaction -> String -> String -> Html Msg
interactionButton foodId _ _ interaction emoji label =
    button
        [ class "flex w-full items-center gap-4 rounded-[26px] bg-[#eef2e6] px-4 py-3 text-left transition active:scale-[0.99] active:bg-[#e5ecd7]"
        , onClick (StartLog foodId interaction)
        ]
        [ div [ class "emoji grid h-12 w-12 shrink-0 place-items-center rounded-full bg-white text-[24px] shadow-sm sm:h-14 sm:w-14 sm:text-[28px]" ] [ text emoji ]
        , div [ class "min-w-0" ]
            [ span [ class "block text-[18px] font-semibold text-slate-700 sm:text-[20px]" ] [ text label ]
            , span [ class "block text-xs font-semibold uppercase tracking-[0.24em] text-slate-500" ] [ text "uses selected prep style" ]
            ]
        ]


emojiChoiceGrid : String -> List FoodChoice -> Html Msg
emojiChoiceGrid selectedEmoji choices =
    ul [ class "mt-5 grid list-none grid-cols-4 gap-4 p-0" ]
        (List.map (foodChoiceChip selectedEmoji) choices)


foodChoiceChip : String -> FoodChoice -> Html Msg
foodChoiceChip selectedEmoji choice =
    li
        [ classList
            [ ( "grid h-16 w-16 place-items-center rounded-full text-[32px] transition", True )
            , ( "bg-[#eef1e7]", choice.emoji /= selectedEmoji )
            , ( "bg-[#d9efb8] ring-2 ring-[#7fb83a]", choice.emoji == selectedEmoji )
            ]
        , onClick (UpdateDraftEmoji choice.emoji)
        ]
        [ span [ class "emoji" ] [ text choice.emoji ]
        , span [ class "sr-only" ] [ text choice.name ]
        ]


foodSummary : Food -> String
foodSummary food =
    let
        counts =
            interactionCounts food.logs
    in
    case food.tier of
        Mastered ->
            "Mastered by repeated eating. Keep checking it in every few weeks to make sure it stays easy."

        Shelved ->
            "This one is paused for now. Return it when you want a lower-pressure meal."

        Active ->
            if masteryStreak food.logs >= 2 then
                "Close to mastery. The child is showing real eating momentum, not just familiarizing."

            else if counts.tasteCount > 0 || counts.touchCount > 0 then
                "Building comfort through contact and tasting. Still in the learning phase."

            else
                "Fresh on the radar. Start with look, touch, or taste and let the momentum build."


maintenanceCopy : Bool -> String
maintenanceCopy stale =
    if stale then
        "Faded a bit. Time for a maintenance exposure."

    else
        "Steady and reliable."


tierBadge : Tier -> Html Msg
tierBadge tier =
    case tier of
        Active ->
            span [ class "rounded-full bg-[#eef6da] px-3 py-1 text-[11px] font-extrabold uppercase tracking-[0.24em] text-[#4d7d00]" ]
                [ text "Active" ]

        Shelved ->
            span [ class "rounded-full bg-[#f0efe8] px-3 py-1 text-[11px] font-extrabold uppercase tracking-[0.24em] text-slate-500" ]
                [ text "Shelved" ]

        Mastered ->
            span [ class "rounded-full bg-[#dff2b6] px-3 py-1 text-[11px] font-extrabold uppercase tracking-[0.24em] text-[#446f00]" ]
                [ text "Mastered" ]


foodIcon : Food -> Html Msg
foodIcon food =
    div
        [ class "emoji grid h-16 w-16 shrink-0 place-items-center rounded-full bg-[#ffb35f] text-[34px]" ]
        [ text food.emoji ]


avatar : Html Msg
avatar =
    div
        [ class "emoji grid h-12 w-12 place-items-center rounded-full bg-gradient-to-br from-orange-200 to-orange-400 text-[26px] shadow-sm" ]
        [ text "👧" ]


interactionCounts : List FoodLog -> { lookCount : Int, touchCount : Int, smellCount : Int, tasteCount : Int, eatCount : Int }
interactionCounts logs =
    List.foldl
        (\log counts ->
            case log.interaction of
                Look ->
                    { counts | lookCount = counts.lookCount + 1 }

                Touch ->
                    { counts | touchCount = counts.touchCount + 1 }

                Smell ->
                    { counts | smellCount = counts.smellCount + 1 }

                Taste ->
                    { counts | tasteCount = counts.tasteCount + 1 }

                Eat ->
                    { counts | eatCount = counts.eatCount + 1 }
        )
        { lookCount = 0, touchCount = 0, smellCount = 0, tasteCount = 0, eatCount = 0 }
        logs


masteryStreak : List FoodLog -> Int
masteryStreak logs =
    case logs of
        log :: rest ->
            if log.interaction == Eat then
                1 + masteryStreak rest

            else
                0

        [] ->
            0


recencyLabel : Int -> Maybe Int -> String
recencyLabel now maybeAt =
    case maybeAt of
        Nothing ->
            "Not yet logged"

        Just at ->
            let
                days =
                    max 0 ((now - at) // dayMs)
            in
            if days <= 0 then
                "Today"

            else if days == 1 then
                "1 day ago"

            else if days < 7 then
                String.fromInt days ++ " days ago"

            else if days < 30 then
                String.fromInt ((days + 3) // 7) ++ " weeks ago"

            else
                "Long ago"


needsMaintenance : Int -> Food -> Bool
needsMaintenance now food =
    case foodLastLoggedAt food of
        Nothing ->
            food.tier == Mastered

        Just at ->
            food.tier == Mastered && ((now - at) // dayMs) >= 21


foodLastLoggedAt : Food -> Maybe Int
foodLastLoggedAt food =
    case food.logs of
        log :: _ ->
            Just log.at

        [] ->
            Nothing


sortActiveFoods : List Food -> List Food
sortActiveFoods foods =
    List.sortWith
        (\a b ->
            compare (lastSortKey a) (lastSortKey b)
        )
        (List.filter (\food -> food.tier == Active) foods)


lastSortKey : Food -> Int
lastSortKey food =
    Maybe.withDefault 0 (foodLastLoggedAt food)


recentLogItems :
    List Food
    ->
        List
            { at : Int
            , food : String
            , interaction : Interaction
            , prepStyle : PrepStyle
            , note : String
            }
recentLogItems foods =
    foods
        |> List.concatMap
            (\food ->
                List.map
                    (\log ->
                        { at = log.at
                        , food = food.name
                        , interaction = log.interaction
                        , prepStyle = log.prepStyle
                        , note = log.note
                        }
                    )
                    food.logs
            )
        |> List.sortWith (\a b -> compare b.at a.at)


applyPendingLog : Int -> PendingLog -> List Food -> List Food
applyPendingLog now pending foods =
    updateFoodById
        pending.foodId
        (\food ->
            let
                nextLog =
                    { at = now
                    , interaction = pending.interaction
                    , prepStyle = pending.prepStyle
                    , note = pending.note
                    }

                nextLogs =
                    nextLog :: food.logs

                nextTier =
                    if masteryStreak nextLogs >= 3 then
                        Mastered

                    else if food.tier == Shelved then
                        Active

                    else
                        food.tier
            in
            { food | logs = nextLogs, tier = nextTier }
        )
        foods


nextFoodId : List Food -> Int
nextFoodId foods =
    (List.foldl (\food currentMax -> max food.id currentMax) 0 foods) + 1


updateFoodById : Int -> (Food -> Food) -> List Food -> List Food
updateFoodById foodId transform foods =
    List.map
        (\food ->
            if food.id == foodId then
                transform food

            else
                food
        )
        foods


foodById : Int -> List Food -> Maybe Food
foodById foodId foods =
    List.filter (\food -> food.id == foodId) foods
        |> List.head


dayMs : Int
dayMs =
    24 * 60 * 60 * 1000


currentNow : Model -> Int
currentNow model =
    Maybe.withDefault 0 model.currentTime


foodEncoder : Food -> Encode.Value
foodEncoder food =
    Encode.object
        [ ( "id", Encode.int food.id )
        , ( "name", Encode.string food.name )
        , ( "emoji", Encode.string food.emoji )
        , ( "category", Encode.string food.category )
        , ( "tier", tierEncoder food.tier )
        , ( "createdAt", Encode.int food.createdAt )
        , ( "logs", Encode.list foodLogEncoder food.logs )
        ]


foodDecoder : Decoder Food
foodDecoder =
    Decode.oneOf
        [ Decode.map7
            (\id name emoji category tier createdAt logs ->
                { id = id
                , name = name
                , emoji = emoji
                , category = category
                , tier = tier
                , createdAt = createdAt
                , logs = logs
                }
            )
            (Decode.field "id" Decode.int)
            (Decode.field "name" Decode.string)
            (Decode.field "emoji" Decode.string)
            (Decode.oneOf
                [ Decode.field "category" Decode.string
                , Decode.succeed "Custom"
                ]
            )
            (Decode.oneOf
                [ Decode.field "tier" tierDecoder
                , Decode.succeed Active
                ]
            )
            (Decode.oneOf
                [ Decode.field "createdAt" Decode.int
                , Decode.succeed 0
                ]
            )
            (Decode.oneOf
                [ Decode.field "logs" (Decode.list foodLogDecoder)
                , Decode.succeed []
                ]
            )
        , legacyFoodDecoder
        ]


legacyFoodDecoder : Decoder Food
legacyFoodDecoder =
    Decode.map7
        (\id name emoji category exposures highestStage maybeInteraction ->
            let
                legacyLogs =
                    case maybeInteraction of
                        Just interaction ->
                            [ { at = 0
                              , interaction = interaction
                              , prepStyle = Raw
                              , note = "Imported from the previous exposure tracker"
                              }
                            ]

                        Nothing ->
                            []
            in
            { id = id
            , name = name
            , emoji = emoji
            , category = category
            , tier =
                case highestStage of
                    MasteredStage ->
                        Mastered

                    _ ->
                        if exposures >= 12 then
                            Mastered

                        else
                            Active
            , createdAt = 0
            , logs = legacyLogs
            }
        )
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "emoji" Decode.string)
        (Decode.field "category" Decode.string)
        (Decode.oneOf
            [ Decode.field "exposures" Decode.int
            , Decode.succeed 0
            ]
        )
        (Decode.oneOf
            [ Decode.field "highestStage" stageDecoder
            , Decode.succeed Newbie
            ]
        )
        (Decode.oneOf
            [ Decode.field "lastInteraction" (Decode.nullable interactionDecoder)
            , Decode.succeed Nothing
            ]
        )


foodLogEncoder : FoodLog -> Encode.Value
foodLogEncoder log =
    Encode.object
        [ ( "at", Encode.int log.at )
        , ( "interaction", interactionEncoder log.interaction )
        , ( "prepStyle", prepStyleEncoder log.prepStyle )
        , ( "note", Encode.string log.note )
        ]


foodLogDecoder : Decoder FoodLog
foodLogDecoder =
    Decode.map4 FoodLog
        (Decode.oneOf
            [ Decode.field "at" Decode.int
            , Decode.succeed 0
            ]
        )
        (Decode.oneOf
            [ Decode.field "interaction" interactionDecoder
            , Decode.succeed Look
            ]
        )
        (Decode.oneOf
            [ Decode.field "prepStyle" prepStyleDecoder
            , Decode.succeed Raw
            ]
        )
        (Decode.oneOf
            [ Decode.field "note" Decode.string
            , Decode.succeed ""
            ]
        )


interactionLabel : Interaction -> String
interactionLabel interaction =
    case interaction of
        Look ->
            "Look"

        Touch ->
            "Touch"

        Smell ->
            "Smell"

        Taste ->
            "Taste"

        Eat ->
            "Eat"


interactionEncoder : Interaction -> Encode.Value
interactionEncoder interaction =
    Encode.string
        (case interaction of
            Look ->
                "look"

            Touch ->
                "touch"

            Smell ->
                "smell"

            Taste ->
                "taste"

            Eat ->
                "eat"
        )


interactionDecoder : Decoder Interaction
interactionDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "look" ->
                        Decode.succeed Look

                    "touch" ->
                        Decode.succeed Touch

                    "smell" ->
                        Decode.succeed Smell

                    "taste" ->
                        Decode.succeed Taste

                    "eat" ->
                        Decode.succeed Eat

                    _ ->
                        Decode.fail ("Unknown interaction: " ++ value)
            )


tierEncoder : Tier -> Encode.Value
tierEncoder tier =
    Encode.string
        (case tier of
            Active ->
                "active"

            Shelved ->
                "shelved"

            Mastered ->
                "mastered"
        )


tierDecoder : Decoder Tier
tierDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "active" ->
                        Decode.succeed Active

                    "shelved" ->
                        Decode.succeed Shelved

                    "mastered" ->
                        Decode.succeed Mastered

                    _ ->
                        Decode.fail ("Unknown tier: " ++ value)
            )


prepStyleLabel : PrepStyle -> String
prepStyleLabel prepStyle =
    case prepStyle of
        Raw ->
            "Raw"

        Roasted ->
            "Roasted"

        Steamed ->
            "Steamed"

        Mashed ->
            "Mashed"

        Sliced ->
            "Sliced"

        Mixed ->
            "Mixed"

        Dip ->
            "With dip"

        OtherPrep ->
            "Other"


prepStyleEncoder : PrepStyle -> Encode.Value
prepStyleEncoder prepStyle =
    Encode.string
        (case prepStyle of
            Raw ->
                "raw"

            Roasted ->
                "roasted"

            Steamed ->
                "steamed"

            Mashed ->
                "mashed"

            Sliced ->
                "sliced"

            Mixed ->
                "mixed"

            Dip ->
                "dip"

            OtherPrep ->
                "other"
        )


prepStyleDecoder : Decoder PrepStyle
prepStyleDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "raw" ->
                        Decode.succeed Raw

                    "roasted" ->
                        Decode.succeed Roasted

                    "steamed" ->
                        Decode.succeed Steamed

                    "mashed" ->
                        Decode.succeed Mashed

                    "sliced" ->
                        Decode.succeed Sliced

                    "mixed" ->
                        Decode.succeed Mixed

                    "dip" ->
                        Decode.succeed Dip

                    "other" ->
                        Decode.succeed OtherPrep

                    _ ->
                        Decode.fail ("Unknown prep style: " ++ value)
            )


stageDecoder : Decoder Stage
stageDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "newbie" ->
                        Decode.succeed Newbie

                    "learning" ->
                        Decode.succeed Learning

                    "growing" ->
                        Decode.succeed Growing

                    "mastered" ->
                        Decode.succeed MasteredStage

                    _ ->
                        Decode.fail ("Unknown stage: " ++ value)
            )


defaultFoodNameForEmoji : String -> String
defaultFoodNameForEmoji emoji =
    case emoji of
        "🍎" ->
            "Apple"

        "🍌" ->
            "Banana"

        "🍓" ->
            "Strawberry"

        "🫐" ->
            "Blueberries"

        "🍐" ->
            "Pear"

        "🍊" ->
            "Orange"

        "🍉" ->
            "Watermelon"

        "🥦" ->
            "Broccoli"

        "🥕" ->
            "Carrot"

        "🍠" ->
            "Sweet Potato"

        "🌽" ->
            "Corn"

        "🍝" ->
            "Pasta"

        "🍚" ->
            "Rice"

        "🍞" ->
            "Bread"

        "🥣" ->
            "Oatmeal"

        "🧀" ->
            "Cheese"

        "🥛" ->
            "Milk"

        "🥚" ->
            "Egg"

        "🍗" ->
            "Chicken"

        "🐟" ->
            "Fish"

        "🥑" ->
            "Avocado"

        "🍪" ->
            "Snack"

        _ ->
            "Custom Food"
