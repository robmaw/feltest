module Main

open Feliz
open Feliz.UseElmish
open Feliz.Bulma
open Elmish
open Browser.Dom

type Msg =
  | Increment
  | Decrement

type State = { Count: int }

let init () = { Count = 0 }, Cmd.none

let update msg state =
  match msg with
  | Increment -> { state with Count = state.Count + 1 }, Cmd.none
  | Decrement -> { state with Count = state.Count - 1 }, Cmd.none

[<ReactComponent>]
let Counter () =
  let state, dispatch = React.useElmish (init, update, [||])

  Html.div
    [ Html.h1 state.Count
      Html.button [ prop.text "Increment"; prop.onClick (fun _ -> dispatch Increment) ]

      Html.button [ prop.text "Decrement"; prop.onClick (fun _ -> dispatch Decrement) ] ]

[<ReactComponent>]
let FactCard () =
 Bulma.card [
    Bulma.cardHeader [
        prop.children [
            Bulma.cardHeaderTitle.p "Radar Facts"
        ]
        ]
    Bulma.cardContent [
      Bulma.columns [
        Bulma.column[
          Bulma.cardContent [
            Bulma.title[Html.h3 "Active electronically scanned array"]
            Bulma.block[Html.p "An active electronically scanned array (AESA) is a type of phased array antenna, which is a computer-controlled array antenna in which the beam of radio waves can be electronically steered to point in different directions without moving the antenna."]
          ]
        ]
        Bulma.column[
          Bulma.cardImage [
            Bulma.image [
              Bulma.image.isFullWidth
              prop.children [
                Html.img [
                  prop.alt "Placeholder image"
                  prop.src "https://upload.wikimedia.org/wikipedia/commons/5/59/3DELRR_long-range_radar_system.JPG"
                ]
              ]
            ]
          ]
        ]
      ]
    ]
    Bulma.cardFooter [
        Bulma.cardFooterItem.a [
            prop.text "Refresh"
        ]
    ]
 ]


[<ReactComponent>]
let Card () =
  Bulma.card
    [ Bulma.cardImage
        [ Bulma.image
            [ Bulma.image.is4by3
              prop.children
                [ Html.img
                    [ prop.alt "Placeholder image"
                      prop.src "https://bulma.io/images/placeholders/1280x960.png" ] ] ] ]
      Bulma.cardContent
        [ Bulma.media
              [ Bulma.mediaLeft
                    [ Bulma.cardImage
                          [ Bulma.image
                                [ Bulma.image.is48x48
                                  prop.children
                                    [ Html.img
                                        [ prop.alt "Placeholder image"
                                          prop.src "https://bulma.io/images/placeholders/96x96.png" ] ] ] ] ]
                Bulma.mediaContent
                    [ Bulma.title.p [ Bulma.title.is4; prop.text "Feliz Bulma" ]
                      Bulma.subtitle.p [ Bulma.title.is6; prop.text "@feliz.bulma" ] ] ]
          Bulma.content "Lorem ipsum dolor sit ... nec iaculis mauris." ] ]



[<ReactComponent>]
let LayoutTest () =
  Bulma.container
    [ 
      container.isFluid
      prop.children[
        FactCard()
      ]
    ]




let root = ReactDOM.createRoot (document.getElementById "feliz-app")
root.render (LayoutTest())
