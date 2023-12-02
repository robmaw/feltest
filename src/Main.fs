module Main

open Feliz
open Feliz.UseElmish
open Feliz.Bulma
open Elmish
open Browser.Dom
open Fable.SimpleHttp
open Thoth.Json

type AsyncOperationStatus<'t> =
    | Started
    | Finished of 't

type Fact = {
    Title:string
    Summary:string
    Image:string
}

type Msg =
  | Increment
  | Decrement
  | GetFact of AsyncOperationStatus<Result<Fact, string>>
  | FetchData
  | FetchDataSuccess of string
  | FetchDataFailure of exn
  | OnError of System.Exception

type State =  {
  Count: int
  Fact: Deferred.Deferred<Result<Fact, string>>
  Loading:bool
  Result: string option
}

let runAfter (ms: int) callback =
  async {
    do! Async.Sleep ms
    do callback()
  }
  |> Async.StartImmediate

let placeholderFact = {
    Title = "Loading..."
    Summary = "Loading..."
    Image = "https://bulma.io/images/placeholders/1280x960.png"
}

let init () = 
  { 
      Count = 0
      Fact = Deferred.HasNotStartedYet
      Loading = false
      Result = None
  }, Cmd.none

let update msg state =
  match msg with
  | Increment -> { state with Count = state.Count + 1 }, Cmd.none
  | Decrement -> { state with Count = state.Count - 1 }, Cmd.none
  | GetFact Started when state.Fact = Deferred.InProgress -> state, Cmd.none
  | GetFact Started -> 
      let nextState = { state with Fact = Deferred.InProgress; Loading=true }
      let getFact() = async 
                        {
                          let! (statusCode, rawFact) = Http.get "https://radar.thoughtworks.com/api/publish/facts"
                          match statusCode with
                          | 200 -> 
                              let factResult = (Decode.Auto.fromString<Fact> rawFact)
                              return (Finished (factResult))
                          | _ ->
                              return (Finished (Error rawFact))
                        } 
      nextState, Cmd.OfAsync.either getFact () GetFact OnError
  | FetchData ->
      state, Cmd.OfAsync.attempt (fun _ -> Http.get "/api/data") () FetchDataFailure
  | FetchDataSuccess data ->
      state, Cmd.none
  | FetchDataFailure error ->
      state, Cmd.none
  | GetFact (Finished (Ok fact)) -> 
      let nextState = { state with Fact = Deferred.Resolved (Ok fact); Loading=false }
      nextState, Cmd.none
  | GetFact (Finished (Error error)) -> 
      let nextState = { state with Fact = Deferred.Resolved(Error error); Loading=false ; Result = Some error}
      nextState, Cmd.none
  | OnError ex->
      Fable.Core.JS.console.error $"Error: {ex.Message}"
      state, Cmd.none

[<ReactComponent>]
let Counter () =
  let state, dispatch = React.useElmish (init, update, [||])

  Html.div
    [ Html.h1 state.Count
      Html.button [ prop.text "Increment"; prop.onClick (fun _ -> dispatch Increment) ]

      Html.button [ prop.text "Decrement"; prop.onClick (fun _ -> dispatch Decrement) ] ]

[<ReactComponent>]
let FactCard () =
 let state, dispatch = React.useElmish (init, update, [||])
 let progress =
    match state.Fact with
    | Deferred.InProgress -> 
        Bulma.progress [
            Bulma.color.isPrimary
            prop.max 100
        ]
    | _ -> Bulma.content []

 
 Bulma.card [
    //cardHeader
    Bulma.cardHeader [
        prop.role "banner"
        prop.children [
            Bulma.container [
              Bulma.columns [
                columns.isVCentered
                prop.children [
                  Bulma.column [
                    column.isOneThird
                    prop.children [
                      Bulma.cardHeaderTitle.p "Radar Facts"
                    ]
                  ]
                  Bulma.column [
                    progress
                  ]
                ]
              ]
            ]
        ]
    ]
    //cardContent
    Html.main [
      prop.role "main"
      prop.children [
        Bulma.cardContent [
          Bulma.columns [
            Bulma.column[
              Bulma.cardContent [
                Bulma.title[Html.h1 ($"{placeholderFact.Title} {state.Count}")]
                Bulma.block[Html.p $"{placeholderFact.Summary}"]
              ]
            ]
            Bulma.column[
              Bulma.cardImage [
                Bulma.image [
                  Bulma.image.isFullWidth
                  prop.children [
                    Html.img [
                      prop.alt "Placeholder image"
                      prop.src placeholderFact.Image
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
    //cardFooter
    Bulma.cardFooter [
        Bulma.cardFooterItem.div [
            prop.children[
              Bulma.button.button
                [ prop.text "Get Fact"
                  prop.onClick (fun _ -> dispatch (GetFact Started))
                  prop.disabled (Deferred.inProgress state.Fact) 
                ]
            ]
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
