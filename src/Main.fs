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
  | GetFact of AsyncOperationStatus<Result<Fact, string>>
  | ImageLoaded
  | ImageError
  | OnError of System.Exception

type State =  {
  Fact: Deferred.Deferred<Result<Fact, string>>
  ImageLoading: Deferred.Deferred<bool>
}

let placeholderFact = {
    Title = "Loading..."
    Summary = "Loading..."
    Image = "./img/search.gif"
}

let init () = 
  { 
      Fact = Deferred.HasNotStartedYet
      ImageLoading = Deferred.HasNotStartedYet
  }, Cmd.none

let update msg state =
  match msg with
  | GetFact Started when state.Fact = Deferred.InProgress -> state, Cmd.none
  | GetFact Started -> 
      let nextState = { state with Fact = Deferred.InProgress; ImageLoading=Deferred.InProgress }
      let getFact() = async 
                        {
                          let! (statusCode, rawFact) = Http.get "http://127.0.0.1:8080"
                          match statusCode with
                          | 200 -> 
                              let factResult = (Decode.Auto.fromString<Fact> rawFact)
                              match factResult with
                              | Ok fact ->  Fable.Core.JS.console.log fact.Title
                              | _ -> ()
                              return (Finished (factResult))
                          | code ->
                              return (Finished (Error $"Error: {code}"))
                        } 
      nextState, Cmd.OfAsync.either getFact () GetFact OnError
  | GetFact (Finished (Ok fact)) -> 
      let nextState = { state with Fact = Deferred.Resolved (Ok fact)}
      nextState, Cmd.none
  | GetFact (Finished (Error error)) -> 
      let err = $"Error: {error}"
      let nextState = { state with Fact = Deferred.Resolved(Error error)}
      Fable.Core.JS.console.log err
      nextState, Cmd.none
  | ImageLoaded ->
      let nextState = { state with ImageLoading = Deferred.Resolved(true)}
      nextState, Cmd.none
  | ImageError ->
      let nextState = { state with ImageLoading = Deferred.Resolved(false)}
      nextState, Cmd.none
  | OnError ex->
      let error = $"Error: {ex.Message}"
      Fable.Core.JS.console.log error
      let nextState = { state with Fact = Deferred.Resolved(Error error)}
      nextState, Cmd.none

let progress state =
  let progressBar = 
    match state.Fact with
    | Deferred.InProgress -> Bulma.content[
        Bulma.progress [
            Bulma.color.isPrimary
            prop.max 100
        ]
      ]
    | Deferred.Resolved (Error error) -> Bulma.content [
        Bulma.progress [
            Bulma.color.isWarning
            prop.max 100
            prop.value 100
        ]
        Html.p error
      ]
    | _ -> Bulma.content []
  Bulma.content [
    spacing.pr5
    spacing.pt3
    prop.children [
      progressBar
    ]
  ]

let resolveFact state =
  match state.Fact with
  | Deferred.Resolved (Ok fact) -> fact
  | _ -> placeholderFact

let resolveImage state =
  match state.ImageLoading with
  | Deferred.Resolved (true) -> (resolveFact state).Image
  | Deferred.Resolved (false) -> placeholderFact.Image
  | Deferred.InProgress -> placeholderFact.Image
  | _ -> placeholderFact.Image

[<ReactComponent>]
let FactCard () =
  let state, dispatch = React.useElmish (init, update, [||])
  Bulma.card [
    //cardHeader
    Bulma.cardHeader [
        prop.role "banner"
        prop.children [
          Bulma.container [
            container.isFluid
            helpers.isPaddingless
            prop.children [
              Bulma.columns[
                prop.children [
                  Bulma.column [
                    column.isHalf
                    helpers.isPulledLeft
                    prop.children [
                      Bulma.cardHeaderTitle.div [
                        prop.children [
                          Html.p "Fact"
                        ]
                      ]
                    ]
                  ]
                  Bulma.column [
                    column.isHalf                    
                  ]
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
                spacing.pl0
                prop.children[
                  Bulma.title[Html.p (resolveFact state).Title]
                  Bulma.block[Html.p (resolveFact state).Summary]
                ]
              ]
            ]
            Bulma.column[
              Bulma.cardImage [
                Bulma.image [
                  Bulma.image.isFullWidth
                  prop.children [
                    Html.img [
                      prop.alt "Placeholder image"
                      prop.src (resolveImage state)
                      // prop.style [ style.display.none ]
                      prop.onLoad (fun _ -> dispatch ImageLoaded)
                      prop.onError (fun (_:Browser.Types.Event) -> dispatch ImageError)
                      prop.onError (fun (_:Browser.Types.UIEvent) -> dispatch ImageError)
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
              progress state
            ]
        ]
    ]
  ]


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
