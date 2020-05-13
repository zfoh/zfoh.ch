port module Projects exposing (main)

import Browser
import Html exposing (Attribute, Html, a, div, text)
import Html.Attributes as HtmlA exposing (class, href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as P
import Json.Encode as Encode
import Set exposing (Set)
import String.Extra
import Table exposing (defaultCustomizations)


port save : Value -> Cmd msg


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { projects : List Project
    , bookmarkedProjects : Set ProjectId
    , selBeginner : Bool
    , selIntermediate : Bool
    , selAdvanced : Bool
    , tableState : Table.State
    , error : Maybe String
    }


type alias ProjectId =
    String


type alias ContribLevels =
    { beginner : Bool
    , intermediate : Bool
    , advanced : Bool
    }


contribLevelsDecoder : Decoder ContribLevels
contribLevelsDecoder =
    Decode.succeed ContribLevels
        |> P.optional "beginner" Decode.bool False
        |> P.optional "intermediate" Decode.bool False
        |> P.optional "advanced" Decode.bool False


type alias Project =
    { id : ProjectId
    , name : String
    , link : String
    , contributorLevel : ContribLevels
    , contact : String
    , description : String
    }


readProjects : Cmd Msg
readProjects =
    Http.get
        { url = "./projects.json"
        , expect = Http.expectJson GotProjects (Decode.list projectDecoder)
        }


projectDecoder : Decoder Project
projectDecoder =
    Decode.succeed Project
        |> P.required "id" Decode.string
        |> P.required "name" Decode.string
        |> P.optional "link" Decode.string ""
        |> P.required "contributor level" contribLevelsDecoder
        |> P.required "contact" Decode.string
        |> P.optional "description" Decode.string ""


init : List ProjectId -> ( Model, Cmd Msg )
init bookmarkedProjects =
    let
        model =
            { projects = []
            , bookmarkedProjects = Set.fromList bookmarkedProjects
            , selBeginner = True
            , selIntermediate = True
            , selAdvanced = True
            , tableState = Table.initialSort "Name"
            , error = Nothing
            }
    in
    ( model, readProjects )


type Msg
    = ToggleSelected ProjectId
    | SetTableState Table.State
    | GotProjects (Result Http.Error (List Project))
    | ToggleContribLevel String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ bookmarkedProjects } as model) =
    case msg of
        GotProjects (Ok projects) ->
            ( { model | projects = projects }
            , Cmd.none
            )

        GotProjects (Err err) ->
            ( { model | error = Just (httpErrorToString err) }
            , Cmd.none
            )

        ToggleSelected id ->
            let
                newBookmarkedProjects =
                    if Set.member id bookmarkedProjects then
                        Set.remove id bookmarkedProjects

                    else
                        Set.insert id bookmarkedProjects

                cmd =
                    Set.toList newBookmarkedProjects
                        |> Encode.list Encode.string
                        |> save
            in
            ( { model | bookmarkedProjects = newBookmarkedProjects }
            , cmd
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        ToggleContribLevel level ->
            case level of
                "beginner" ->
                    ( { model | selBeginner = not model.selBeginner }
                    , Cmd.none
                    )

                "intermediate" ->
                    ( { model | selIntermediate = not model.selIntermediate }
                    , Cmd.none
                    )

                _ ->
                    ( { model | selAdvanced = not model.selAdvanced }
                    , Cmd.none
                    )


view : Model -> Html Msg
view ({ projects, tableState, bookmarkedProjects, error } as model) =
    let
        projectIsInFilter p =
            (model.selBeginner && p.contributorLevel.beginner)
                || (model.selIntermediate && p.contributorLevel.intermediate)
                || (model.selAdvanced && p.contributorLevel.advanced)

        selectedProjects =
            List.filter projectIsInFilter projects
    in
    div []
        [ case error of
            Nothing ->
                text ""

            Just e ->
                Html.h1 [] [ text e ]
        , div [ HtmlA.style "margin-bottom" "1rem" ]
            [ levelCheckbox model "beginner"
            , levelCheckbox model "intermediate"
            , levelCheckbox model "advanced"
            ]
        , Table.view
            (tableConfig bookmarkedProjects)
            tableState
            selectedProjects
        ]


levelCheckbox : Model -> String -> Html Msg
levelCheckbox model level =
    let
        accessor =
            case level of
                "beginner" ->
                    .selBeginner

                "intermediate" ->
                    .selIntermediate

                _ ->
                    .selAdvanced

        htmlId =
            "select-" ++ level
    in
    div []
        [ Html.input
            [ HtmlA.type_ "checkbox"
            , HtmlA.checked (accessor model)
            , onClick (ToggleContribLevel level)
            , HtmlA.id htmlId
            ]
            []
        , Html.label [ HtmlA.for htmlId ] [ text <| String.Extra.humanize level ]
        ]


tableConfig : Set ProjectId -> Table.Config Project Msg
tableConfig selectedIds =
    Table.customConfig
        { toId = .name
        , toMsg = SetTableState
        , columns =
            [ infoColumn selectedIds
            , stringColumnUnsortable "Contact" .contact
            , contribColumn
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = [ class "projects" ]
                , thead = simpleThead
                , rowAttrs = toRowAttrs selectedIds
            }
        }


toRowAttrs : Set ProjectId -> Project -> List (Attribute Msg)
toRowAttrs selectedIds p =
    [ class
        (if Set.member p.id selectedIds then
            "selected"

         else
            ""
        )
    ]


infoColumn : Set ProjectId -> Table.Column Project Msg
infoColumn selectedIds =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = viewInfo selectedIds
        , sorter = Table.unsortable
        }


contribColumn : Table.Column Project Msg
contribColumn =
    let
        contribAttr isEnabled =
            if isEnabled then
                HtmlA.style "opacity" "1"

            else
                HtmlA.style "opacity" "0.2"

        viewLevels : ContribLevels -> Table.HtmlDetails Msg
        viewLevels lvls =
            Table.HtmlDetails [ class "font-mono font-bold" ]
                [ Html.span [ contribAttr lvls.beginner ] [ text "B " ]
                , Html.span [ contribAttr lvls.intermediate ] [ text "I " ]
                , Html.span [ contribAttr lvls.advanced ] [ text "A " ]
                ]
    in
    Table.veryCustomColumn
        { name = "Levels"
        , viewData = .contributorLevel >> viewLevels
        , sorter = Table.unsortable
        }


stringColumnUnsortable : String -> (data -> String) -> Table.Column data msg
stringColumnUnsortable name toStr =
    Table.customColumn
        { name = name
        , viewData = toStr
        , sorter = Table.unsortable
        }


viewInfo : Set ProjectId -> Project -> Table.HtmlDetails Msg
viewInfo selectedIds p =
    let
        iconFile =
            if Set.member p.id selectedIds then
                "projects/bookmark-solid.svg"

            else
                "projects/bookmark-regular.svg"
    in
    Table.HtmlDetails
        []
        [ Html.div [ class "anchor", HtmlA.id p.id ] []
        , Html.img
            [ HtmlA.src iconFile
            , onClick (ToggleSelected p.id)
            , HtmlA.class "clickable bookmark"
            ]
            []
        , case p.link of
            "" ->
                Html.span [ class "name" ] [ text p.name ]

            _ ->
                a [ class "name", href p.link ] [ text p.name ]
        , Html.p [] [ text p.description ]
        ]


simpleThead : List ( String, Table.Status, Attribute msg ) -> Table.HtmlDetails msg
simpleThead headers =
    Table.HtmlDetails [] (List.map simpleTheadHelp headers)


simpleTheadHelp : ( String, Table.Status, Attribute msg ) -> Html msg
simpleTheadHelp ( name, status, click ) =
    let
        title =
            text name

        content =
            case status of
                Table.Unsortable ->
                    [ title ]

                Table.Sortable selected ->
                    [ title
                    , if selected then
                        text " ⇗ "

                      else
                        text " ⇗ "
                    ]

                Table.Reversible Nothing ->
                    [ title
                    , text " ⇕ "
                    ]

                Table.Reversible (Just isReversed) ->
                    [ title
                    , text
                        (if isReversed then
                            " ⇘ "

                         else
                            " ⇗ "
                        )
                    ]
    in
    Html.th [ click ] content


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl text ->
            "Bad Url: " ++ text

        Http.Timeout ->
            "Http Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus code ->
            "Bad Http Status: " ++ String.fromInt code

        Http.BadBody reason ->
            "Bad Http Body: " ++ reason
