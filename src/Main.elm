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


type alias Model =
    { activeTab : Tab
    , overlay : Overlay
    , foods : List Food
    , nextFoodId : Int
    , draftName : String
    , draftEmoji : String
    , storageReady : Bool
    }


type Tab
    = Tracker
    | History
    | Settings


type Overlay
    = NoOverlay
    | AddFood
    | Detail Int


type alias Food =
    { id : Int
    , name : String
    , emoji : String
    , category : String
    , exposures : Int
    , goal : Int
    , highestStage : Stage
    }


type Stage
    = Newbie
    | Learning
    | Growing
    | Mastered


defaultFoods : List Food
defaultFoods =
    [ foodSeed 1 "Broccoli" "🥦" "Vegetable" 8
    , foodSeed 2 "Sweet Potato" "🍠" "Root vegetable" 3
    , foodSeed 3 "Blueberries" "🫐" "Fruit" 12
    , foodSeed 4 "Banana" "🍌" "Fruit" 15
    , foodSeed 5 "Avocado" "🥑" "Healthy fat" 15
    , foodSeed 6 "Oatmeal" "🥣" "Breakfast" 15
    ]


foodSeed : Int -> String -> String -> String -> Int -> Food
foodSeed id name emoji category exposures =
    { id = id
    , name = name
    , emoji = emoji
    , category = category
    , exposures = exposures
    , goal = 15
    , highestStage = stageFromExposures exposures
    }


initialModel : Model
initialModel =
    { activeTab = Tracker
    , overlay = NoOverlay
    , foods = defaultFoods
    , nextFoodId = nextFoodId defaultFoods
    , draftName = ""
    , draftEmoji = "🍎"
    , storageReady = False
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
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
    | CreateFood
    | IncrementExposure Int
    | DeleteFood Int
    | ResetFoods
    | ReceiveFoods Decode.Value


subscriptions : Model -> Sub Msg
subscriptions _ =
    foodsLoaded ReceiveFoods


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab tab ->
            ( { model | activeTab = tab, overlay = NoOverlay }, Cmd.none )

        OpenAddFood ->
            ( { model | overlay = AddFood, draftName = "", draftEmoji = "🍎" }, Cmd.none )

        OpenDetail foodId ->
            ( { model | overlay = Detail foodId }, Cmd.none )

        CloseOverlay ->
            ( { model | overlay = NoOverlay }, Cmd.none )

        UpdateDraftName draftName ->
            ( { model | draftName = draftName }, Cmd.none )

        UpdateDraftEmoji draftEmoji ->
            ( { model | draftEmoji = draftEmoji }, Cmd.none )

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
                    , exposures = 0
                    , goal = 15
                    , highestStage = stageFromExposures 0
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
                        , overlay = NoOverlay
                    }

        IncrementExposure foodId ->
            let
                updatedFoods =
                    updateFoodById
                        foodId
                        (\food ->
                            let
                                nextExposures =
                                    food.exposures + 1
                            in
                            { food
                                | exposures = nextExposures
                                , highestStage = stageFromExposures nextExposures
                            }
                        )
                        model.foods
            in
            persist { model | foods = updatedFoods, overlay = NoOverlay }

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
      }
    , clearFoods ()
    )


view : Model -> Html Msg
view model =
    div
        [ class "relative min-h-screen overflow-hidden text-slate-800"
        , style "background" "radial-gradient(circle at top, rgba(111, 142, 70, 0.14), transparent 30%), linear-gradient(180deg, #f8f6ef 0%, #f3f1e6 100%)"
        ]
        [ div
            [ class "mx-auto flex min-h-screen w-full max-w-[440px] flex-col px-4 pb-28 pt-4" ]
            [ headerView model
            , mainView model
            , bottomNav model
            , floatingAction model
            ]
        , overlayView model
        ]


headerView : Model -> Html Msg
headerView model =
    let
        title =
            case model.activeTab of
                Tracker ->
                    "Toddler Bites"

                History ->
                    "Toddler Bites"

                Settings ->
                    "Toddler Bites"
    in
    section
        [ class "flex items-center justify-between pb-6" ]
        [ div [ class "flex items-center gap-3" ]
            [ avatar
            , h1 [ class "text-[28px] font-extrabold tracking-tight text-[#4f7f08]" ]
                [ text title ]
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
            historyView

        Settings ->
            settingsView


trackerView : Model -> Html Msg
trackerView model =
    let
        activeFoods =
            foodsInProgress model.foods

        masteredFoods =
            masteredFoodsList model.foods
    in
    div [ class "flex flex-1 flex-col gap-8" ]
        [ heroBanner activeFoods masteredFoods
        , section [ class "space-y-4" ]
            [ sectionHeading "Active Learning" "Building a diverse palate"
            , if List.isEmpty activeFoods then
                emptyState "No active foods yet" "Add a new food to start tracking exposures."

              else
                div [ class "space-y-5" ] (List.map activeFoodCard activeFoods)
            ]
        , section [ class "space-y-4" ]
            [ sectionHeading "Mastered Palette" "Confident favorites that keep growing"
            , masteredPalette masteredFoods
            ]
        ]


heroBanner : List Food -> List Food -> Html Msg
heroBanner activeFoods masteredFoods =
    article
        [ class "rounded-[38px] bg-[linear-gradient(135deg,#4b8a00_0%,#3c7500_45%,#2b5700_100%)] px-7 py-8 text-white shadow-[0_20px_40px_rgba(72,116,0,0.24)]" ]
        [ p [ class "text-[12px] uppercase tracking-[0.35em] text-white/65" ]
            [ text "Weekly adventure" ]
        , h2 [ class "mt-4 text-[38px] font-extrabold leading-none tracking-tight" ]
            [ text (String.fromInt (List.length activeFoods) ++ " Foods in Progress") ]
        , div [ class "mt-6 inline-flex rounded-full bg-white/15 px-4 py-2 text-sm font-semibold text-white/95" ]
            [ text (String.fromInt (List.length masteredFoods) ++ " mastered already 🌱") ]
        ]


sectionHeading : String -> String -> Html Msg
sectionHeading title subtitle =
    div []
        [ h2 [ class "text-[30px] font-extrabold tracking-tight text-slate-800" ] [ text title ]
        , p [ class "mt-1 text-[18px] text-slate-500" ] [ text subtitle ]
        ]


activeFoodCard : Food -> Html Msg
activeFoodCard food =
    let
        progress =
            progressPercent food.exposures food.goal
    in
    article
        [ class "rounded-[30px] bg-white/88 p-5 shadow-[0_8px_30px_rgba(120,120,80,0.10)] ring-1 ring-white/80" ]
        [ div [ class "flex items-start justify-between gap-4" ]
        [ div [ class "flex items-center gap-4" ]
                [ foodIcon food
                , div []
                    [ h3 [ class "text-[24px] font-extrabold leading-tight text-slate-800" ] [ text food.name ]
                    , p [ class "mt-1 text-sm uppercase tracking-[0.25em] text-slate-500" ] [ text food.category ]
                    ]
                ]
            , button
                [ class "shrink-0 rounded-full bg-lime-300 px-4 py-3 text-[18px] font-bold text-lime-950 transition hover:bg-lime-200"
                , onClick (OpenDetail food.id)
                ]
                [ text "+ Log Bite" ]
            ]
        , div [ class "mt-5 flex items-center justify-between text-sm font-bold" ]
            [ span [ class "text-[#3f7f09]" ] [ text (String.fromInt food.exposures ++ " Exposures") ]
            , span [ class "text-slate-500" ] [ text ("Stage: " ++ stageLabel food.highestStage) ]
            ]
        , div [ class "mt-2 flex items-center gap-2" ]
            [ span [ class "rounded-full bg-[#e8efd9] px-3 py-1 text-xs font-extrabold uppercase tracking-[0.25em] text-[#3f7f09]" ]
                [ text (stageLabel food.highestStage) ]
            , span [ class "text-xs font-semibold uppercase tracking-[0.22em] text-slate-400" ]
                [ text ("Goal " ++ String.fromInt food.goal) ]
            ]
        , div [ class "mt-3 h-3 overflow-hidden rounded-full bg-[#e2e5d8]" ]
            [ div
                [ class "h-full rounded-full bg-[linear-gradient(90deg,#437e00_0%,#9fe95f_100%)]"
                , style "width" progress
                ]
                []
            ]
        ]


masteredPalette : List Food -> Html Msg
masteredPalette foods =
    div
        [ class "rounded-[36px] bg-[linear-gradient(180deg,#dfe5d7_0%,#e9eee4_100%)] p-5" ]
        [ if List.isEmpty foods then
            emptyState "No mastered foods yet" "Once a food hits 15 exposures, it will appear here."

          else
            div [ class "grid grid-cols-2 gap-4" ]
                (List.map masteredChip foods)
        ]


masteredChip : Food -> Html Msg
masteredChip food =
    article
        [ class "relative rounded-[28px] bg-white p-4 text-center shadow-[0_8px_24px_rgba(86,86,44,0.06)]" ]
        [ div
            [ class "emoji absolute -right-1 -top-1 grid h-8 w-8 place-items-center rounded-full bg-[#417000] text-sm font-bold text-white shadow-md" ]
            [ text "✓" ]
        , div [ class "emoji grid h-16 w-16 place-items-center rounded-full bg-[#fff2ce] text-[34px] shadow-inner" ]
            [ text food.emoji ]
        , h3 [ class "mt-3 text-[19px] font-extrabold text-slate-800" ] [ text food.name ]
        , p [ class "mt-1 text-xs font-bold uppercase tracking-[0.28em] text-slate-500" ]
            [ text (stageLabel food.highestStage) ]
        ]


emptyState : String -> String -> Html Msg
emptyState title body =
    article
        [ class "rounded-[30px] bg-white/82 p-6 text-center shadow-[0_8px_30px_rgba(120,120,80,0.08)] ring-1 ring-white/70" ]
        [ h3 [ class "text-[22px] font-extrabold text-slate-800" ] [ text title ]
        , p [ class "mt-2 text-[16px] leading-7 text-slate-500" ] [ text body ]
        ]


historyView : Html Msg
historyView =
    div [ class "flex flex-1 flex-col gap-6" ]
        [ sectionHeading "History" "Past logs and curious little wins"
        , article [ class "rounded-[34px] bg-white/90 p-5 shadow-[0_8px_30px_rgba(120,120,80,0.10)]" ]
            [ p [ class "text-lg font-semibold text-slate-600" ]
                [ text "Add a history view to see past logs" ]
            , p [ class "mt-2 text-sm leading-7 text-slate-500" ]
                [ text "This is the placeholder structure for a timeline of exposures, reactions, and repeat tries." ]
            ]
        , div [ class "space-y-4" ]
            (List.map historyCard historyItems)
        ]


historyCard :
    { date : String
    , food : String
    , action : String
    , note : String
    }
    -> Html Msg
historyCard item =
    article
        [ class "rounded-[28px] bg-white/86 p-5 shadow-[0_8px_26px_rgba(120,120,80,0.08)] ring-1 ring-white/70" ]
        [ p [ class "text-xs font-bold uppercase tracking-[0.28em] text-slate-500" ] [ text item.date ]
        , h3 [ class "mt-2 text-[22px] font-extrabold text-slate-800" ] [ text item.food ]
        , div [ class "mt-3 flex items-center gap-3" ]
            [ span [ class "rounded-full bg-lime-200 px-3 py-1 text-sm font-semibold text-lime-950" ] [ text item.action ]
            , span [ class "text-sm text-slate-500" ] [ text item.note ]
            ]
        ]


settingsView : Html Msg
settingsView =
    div [ class "flex flex-1 flex-col gap-6 pt-2 pb-6" ]
        [ article
            [ class "relative overflow-hidden rounded-[48px] bg-white px-6 py-8 shadow-[0_18px_42px_rgba(130,120,90,0.12)] ring-1 ring-white/70 sm:px-8 sm:py-10" ]
            [ div
                [ class "absolute left-8 top-10 text-[28px] leading-none sm:left-12 sm:text-[34px]" ]
                [ text "🧹" ]
            , div [ class "mx-auto max-w-[280px] text-center sm:max-w-[320px]" ]
                [ h2 [ class "text-[24px] font-extrabold tracking-tight text-[#1f2d4a] sm:text-[30px]" ]
                    [ text "Reset System" ]
                , p [ class "mt-4 text-[17px] leading-[1.75] text-[#4b5d7f] sm:text-[19px]" ]
                    [ text "Starting a new journey? You can clear all your history here to make room for new milestones." ]
                , button
                    [ class "mt-8 rounded-full bg-[linear-gradient(180deg,#c6ed8a_0%,#b8e57a_100%)] px-6 py-4 text-[14px] font-extrabold tracking-[0.32em] text-[#4c8a00] shadow-[0_10px_18px_rgba(123,173,40,0.18)] sm:px-8 sm:text-[16px]"
                    , onClick ResetFoods
                    ]
                    [ text "FRESH START" ]
                , p [ class "mt-8 text-[17px] leading-[1.8] text-[#4b5d7f] sm:text-[19px]" ]
                    [ text "This will wipe all food tracking data. Useful if starting for a new child or clearing old logs from previous years." ]
                ]
            ]
        , article
            [ class "rounded-[48px] bg-[#f7e6dc] px-6 py-8 shadow-[0_18px_42px_rgba(194,142,111,0.12)] ring-1 ring-white/40 sm:px-8 sm:py-10" ]
            [ div [ class "flex items-start gap-5 sm:gap-7" ]
                [ div
                    [ class "grid h-20 w-20 shrink-0 place-items-center rounded-full bg-white/80 text-[26px] leading-none shadow-[0_8px_22px_rgba(160,129,108,0.10)] sm:h-24 sm:w-24 sm:text-[30px]" ]
                    [ text "🗑️" ]
                , div [ class "min-w-0 flex-1" ]
                    [ h2 [ class "text-[24px] font-extrabold tracking-tight text-[#1f2d4a] sm:text-[30px]" ]
                        [ text "Clear all data?" ]
                    , p [ class "mt-4 max-w-[260px] text-[17px] leading-[1.75] text-[#4b5d7f] sm:max-w-[320px] sm:text-[19px]" ]
                        [ text "Once you reset, your toddler's food history, preferences, and nutritional summaries will be gone forever." ]
                    , button
                        [ class "mt-8 rounded-full bg-[linear-gradient(180deg,#c6ed8a_0%,#b8e57a_100%)] px-6 py-4 text-[14px] font-extrabold tracking-[0.3em] text-[#4c8a00] shadow-[0_10px_18px_rgba(123,173,40,0.18)] sm:px-8 sm:text-[16px]"
                        , onClick ResetFoods
                        ]
                        [ text "RESET ALL DATA" ]
                    , p [ class "mt-8 text-[17px] leading-[1.8] text-[#4b5d7f] sm:text-[19px]" ]
                        [ text "Permanently delete the local tracker state." ]
                    ]
                ]
            ]
        ]


historyItems :
    List
        { date : String
        , food : String
        , action : String
        , note : String
        }
historyItems =
    [ { date = "Today", food = "Broccoli", action = "Taste", note = "8th exposure" }
    , { date = "Today", food = "Blueberries", action = "Look", note = "12th exposure" }
    , { date = "Yesterday", food = "Sweet Potato", action = "Touch", note = "3rd exposure" }
    ]


settingsCard :
    String
    -> String
    -> String
    -> String
    -> String
    -> String
    -> Html Msg
settingsCard title body cta detail emoji wrapperClass =
    article
        [ class ("rounded-[48px] px-6 py-8 shadow-[0_18px_42px_rgba(130,120,90,0.12)] ring-1 ring-white/50 sm:px-8 sm:py-10 " ++ wrapperClass) ]
            [ div [ class "flex items-start gap-5 sm:gap-7" ]
            [ div [ class ("grid h-20 w-20 shrink-0 place-items-center rounded-full bg-white/80 text-[26px] leading-none shadow-[0_8px_22px_rgba(160,129,108,0.10)] sm:h-24 sm:w-24 sm:text-[30px] " ++ if emoji == "🧹" then "bg-transparent shadow-none text-[22px] h-auto w-auto sm:text-[26px]" else "") ]
                [ text emoji ]
            , div [ class "min-w-0 flex-1" ]
                [ h3 [ class "text-[24px] font-extrabold tracking-tight text-[#1f2d4a] sm:text-[30px]" ] [ text title ]
                , p [ class "mt-4 max-w-[260px] text-[17px] leading-[1.75] text-[#4b5d7f] sm:max-w-[320px] sm:text-[19px]" ] [ text body ]
                , button
                    [ class "mt-8 rounded-full bg-[linear-gradient(180deg,#c6ed8a_0%,#b8e57a_100%)] px-6 py-4 text-[14px] font-extrabold tracking-[0.3em] text-[#4c8a00] shadow-[0_10px_18px_rgba(123,173,40,0.18)] sm:px-8 sm:text-[16px]"
                    , onClick ResetFoods
                    ]
                    [ text cta ]
                , p [ class "mt-8 text-[17px] leading-[1.8] text-[#4b5d7f] sm:text-[19px]" ] [ text detail ]
                ]
            ]
        ]


floatingAction : Model -> Html Msg
floatingAction _ =
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
            , tabButton History model.activeTab "History" "🗓️"
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
                    detailOverlay food

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
                    [ h2 [ class "text-[34px] font-extrabold tracking-tight text-slate-800" ] [ text "New Bite" ]
                    , button
                        [ class "grid h-11 w-11 place-items-center rounded-full bg-[#dfe4d8] text-2xl text-slate-700"
                        , onClick CloseOverlay
                        ]
                        [ text "×" ]
                    ]
                , p [ class "mt-5 text-sm font-extrabold uppercase tracking-[0.25em] text-slate-500" ]
                    [ text "Tap to choose an emoji" ]
                , emojiRow model.draftEmoji [ "🍎", "🥦", "🍝", "🍗", "🥛", "🍓", "🍌", "🥑" ]
                , p [ class "mt-8 text-sm font-extrabold uppercase tracking-[0.25em] text-slate-500" ]
                    [ text "Food name" ]
                , input
                    [ class "mt-3 w-full rounded-[30px] bg-[#edf1e2] px-6 py-5 text-xl font-semibold text-slate-700 outline-none placeholder:text-slate-400"
                    , placeholder "What's for yum-yums?"
                    , value model.draftName
                    , onInput UpdateDraftName
                    ]
                    []
                , div [ class "mt-8 flex items-center justify-between" ]
                    [ p [ class "text-sm font-extrabold uppercase tracking-[0.25em] text-slate-500" ] [ text "Selected emoji" ]
                    , span [ class "emoji rounded-full bg-lime-300 px-3 py-1 text-xs font-bold text-[#3f7f09]" ] [ text model.draftEmoji ]
                    ]
                , button
                    [ class "mt-8 w-full rounded-full bg-[linear-gradient(180deg,#497f00_0%,#2f5d00_100%)] py-5 text-[22px] font-extrabold text-white shadow-[0_16px_28px_rgba(58,96,0,0.30)]"
                    , onClick CreateFood
                    ]
                    [ text "Add Food" ]
                ]
            ]
        ]


detailOverlay : Food -> Html Msg
detailOverlay food =
    let
        nextExposure =
            food.exposures + 1
    in
    div
        [ class "fixed inset-0 z-30 bg-black/15 px-3 py-5 backdrop-blur-sm" ]
        [ div
            [ class "mx-auto flex h-full max-w-[420px] items-stretch" ]
            [ article
                [ class "flex w-full flex-col rounded-[36px] bg-[#fbfbf8] p-6 shadow-[0_24px_60px_rgba(72,72,52,0.24)]" ]
                [ button
                    [ class "self-start text-left text-[22px] font-semibold text-slate-600"
                    , onClick CloseOverlay
                    ]
                    [ text "← Cancel" ]
                , div [ class "mt-8 grid place-items-center" ]
                    [ div [ class "emoji grid h-28 w-28 place-items-center rounded-full bg-[#eaf8d9] text-[72px]" ] [ text food.emoji ] ]
                , h2 [ class "mt-7 text-center text-[34px] font-extrabold tracking-tight text-slate-800" ] [ text food.name ]
                , div [ class "mx-auto mt-3 inline-flex rounded-full bg-[#ffcb8e] px-4 py-2 text-sm font-extrabold uppercase tracking-[0.25em] text-[#8a4d00]" ]
                    [ text (String.fromInt food.exposures ++ "th exposure") ]
                , p [ class "mt-4 text-center text-sm font-bold uppercase tracking-[0.28em] text-slate-500" ]
                    [ text ("Stage: " ++ stageLabel food.highestStage) ]
                , p [ class "mt-10 text-center text-[22px] leading-9 text-slate-600" ]
                    [ text "How did they interact with it today?" ]
                , div [ class "mt-8 space-y-4" ]
                    [ interactionRow "👀" "Look"
                    , interactionRow "✋" "Touch"
                    , interactionRow "👃" "Smell"
                    , interactionRow "👅" "Taste"
                    , interactionRow "😋" "Eat"
                    ]
                , button
                    [ class "mt-8 w-full rounded-full bg-[linear-gradient(180deg,#4c8400_0%,#305f00_100%)] py-5 text-[22px] font-extrabold text-white shadow-[0_16px_28px_rgba(58,96,0,0.30)]"
                    , onClick (IncrementExposure food.id)
                    ]
                    [ text ("Log Exposure" ++ (if nextExposure >= food.goal then " and Master It" else "")) ]
                , button
                    [ class "mt-4 w-full rounded-full border-2 border-rose-300 bg-white py-4 text-[18px] font-extrabold text-rose-700"
                    , onClick (DeleteFood food.id)
                    ]
                    [ text "Delete Food" ]
                , p [ class "mt-4 text-center text-sm leading-7 text-slate-500" ]
                    [ text "Consistent exposure leads to food acceptance!" ]
                , article
                    [ class "mt-8 rounded-[32px] bg-[#dff0fb] p-6" ]
                    [ p [ class "text-xs font-extrabold uppercase tracking-[0.25em] text-sky-700" ] [ text "Pro tip" ]
                    , h3 [ class "mt-3 text-[24px] font-extrabold text-slate-800" ] [ text "Keep it low pressure" ]
                    , p [ class "mt-3 text-[18px] leading-8 text-slate-600" ]
                        [ text "If they only want to look today, that's okay! Every interaction counts as progress toward loving their greens." ]
                    ]
                ]
            ]
        ]


interactionRow : String -> String -> Html Msg
interactionRow emoji label =
    div
        [ class "flex items-center gap-4 rounded-[30px] bg-[#eef2e6] px-4 py-4" ]
        [ div [ class "emoji grid h-14 w-14 place-items-center rounded-full bg-white text-[28px] shadow-sm" ] [ text emoji ]
        , span [ class "text-[22px] font-semibold text-slate-700" ] [ text label ]
        ]


emojiRow : String -> List String -> Html Msg
emojiRow selectedEmoji emojis =
    ul [ class "mt-5 grid list-none grid-cols-4 gap-4 p-0" ]
        (List.map (emojiChip selectedEmoji) emojis)


emojiChip : String -> String -> Html Msg
emojiChip selectedEmoji emoji =
    li
        [ classList
            [ ( "grid h-16 w-16 place-items-center rounded-full text-[32px] transition", True )
            , ( "bg-[#eef1e7]", emoji /= selectedEmoji )
            , ( "bg-[#d9efb8] ring-2 ring-[#7fb83a]", emoji == selectedEmoji )
            ]
        , onClick (UpdateDraftEmoji emoji)
        ]
        [ span [ class "emoji" ] [ text emoji ] ]


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


progressPercent : Int -> Int -> String
progressPercent exposures goal =
    let
        ratio =
            if goal <= 0 then
                0

            else
                toFloat exposures / toFloat goal

        clamped =
            clamp 0 1 ratio
    in
    String.fromFloat (clamped * 100) ++ "%"


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


foodsInProgress : List Food -> List Food
foodsInProgress foods =
    List.filter (\food -> food.highestStage /= Mastered) foods


masteredFoodsList : List Food -> List Food
masteredFoodsList foods =
    List.filter (\food -> food.highestStage == Mastered) foods


stageFromExposures : Int -> Stage
stageFromExposures exposures =
    if exposures >= 15 then
        Mastered

    else if exposures >= 5 then
        Growing

    else if exposures >= 1 then
        Learning

    else
        Newbie


stageLabel : Stage -> String
stageLabel stage =
    case stage of
        Newbie ->
            "New"

        Learning ->
            "Learning"

        Growing ->
            "Growing"

        Mastered ->
            "Mastered"


foodEncoder : Food -> Encode.Value
foodEncoder food =
    Encode.object
        [ ( "id", Encode.int food.id )
        , ( "name", Encode.string food.name )
        , ( "emoji", Encode.string food.emoji )
        , ( "category", Encode.string food.category )
        , ( "exposures", Encode.int food.exposures )
        , ( "goal", Encode.int food.goal )
        , ( "highestStage", stageEncoder food.highestStage )
        ]


foodDecoder : Decoder Food
foodDecoder =
    Decode.map6
        (\id name emoji category exposures goal ->
            { id = id
            , name = name
            , emoji = emoji
            , category = category
            , exposures = exposures
            , goal = goal
            , highestStage = stageFromExposures exposures
            }
        )
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "emoji" Decode.string)
        (Decode.field "category" Decode.string)
        (Decode.field "exposures" Decode.int)
        (Decode.field "goal" Decode.int)


stageEncoder : Stage -> Encode.Value
stageEncoder stage =
    Encode.string
        (case stage of
            Newbie ->
                "newbie"

            Learning ->
                "learning"

            Growing ->
                "growing"

            Mastered ->
                "mastered"
        )
