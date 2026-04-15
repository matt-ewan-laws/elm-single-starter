module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import String


type alias Model =
    { count : Int
    }


type Msg
    = Increment
    | Decrement
    | Reset


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { count = 0 }
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }

        Reset ->
            { model | count = 0 }


view : Model -> Html Msg
view model =
    div
        [ class "relative min-h-screen overflow-hidden bg-slate-950 text-slate-100" ]
        [ div [ class "absolute inset-0 bg-[radial-gradient(circle_at_top,_rgba(56,189,248,0.18),_transparent_40%),radial-gradient(circle_at_bottom_right,_rgba(99,102,241,0.16),_transparent_35%)]" ] []
        , div
            [ class "relative mx-auto flex min-h-screen w-full max-w-3xl items-center justify-center px-6 py-16" ]
            [ div
                [ class "w-full rounded-3xl border border-white/10 bg-slate-900/80 p-8 shadow-glow backdrop-blur md:p-12" ]
                [ div [ class "mb-6 inline-flex rounded-full border border-cyan-400/20 bg-cyan-400/10 px-3 py-1 text-sm text-cyan-200" ]
                    [ text "Elm + Tailwind + one HTML file" ]
                , h1 [ class "max-w-xl text-4xl font-semibold tracking-tight text-white md:text-6xl" ]
                    [ text "A tiny starter that builds to a single inline document." ]
                , p [ class "mt-4 max-w-2xl text-base leading-7 text-slate-300 md:text-lg" ]
                    [ text "Edit the Elm view, tweak the Tailwind classes, then run the npm build script to produce one self-contained HTML file." ]
                , div [ class "mt-10 flex flex-wrap items-center gap-4" ]
                    [ button
                        [ onClick Decrement
                        , class "rounded-full border border-white/10 bg-white/5 px-5 py-3 font-medium text-white transition hover:bg-white/10"
                        ]
                        [ text "-" ]
                    , div [ class "min-w-24 rounded-full border border-cyan-400/20 bg-cyan-400/10 px-6 py-3 text-center text-2xl font-semibold text-cyan-200" ]
                        [ text (String.fromInt model.count) ]
                    , button
                        [ onClick Increment
                        , class "rounded-full bg-cyan-400 px-5 py-3 font-medium text-slate-950 transition hover:bg-cyan-300"
                        ]
                        [ text "+" ]
                    , button
                        [ onClick Reset
                        , class "rounded-full border border-white/10 bg-transparent px-5 py-3 font-medium text-slate-200 transition hover:bg-white/5"
                        ]
                        [ text "Reset" ]
                    ]
                , div [ class "mt-8 flex flex-wrap gap-3 text-sm text-slate-400" ]
                    [ pill "Compiled with Elm"
                    , pill "Styled with Tailwind"
                    , pill "Emits one HTML file"
                    ]
                ]
            ]
        ]


pill : String -> Html msg
pill label =
    span
        [ class "rounded-full border border-white/10 bg-white/5 px-3 py-1" ]
        [ text label ]
