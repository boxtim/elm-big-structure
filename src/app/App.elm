module App exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
import Pages.About as About
import Pages.Home as Home
import Pages.NotFound as NotFound
import Url
import Url.Parser as UrlParser exposing (Parser, s, top)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = PathChanged
        , onUrlRequest = LinkClicked
        }



-- PAGE
-- Es necesario definir las páginas en
-- un tipo para efectos de renderizado y
-- conexión de sus tipos internos. Acá
-- se conecta con el modelo local de cada
-- página.


type Page
    = HomePage Home.Model
    | AboutPage About.Model
    | NotFoundPage NotFound.Model


extractModel =
    Tuple.first


pageParser : UrlParser.Parser (Page -> a) a
pageParser =
    UrlParser.oneOf
        [ UrlParser.map (HomePage <| extractModel Home.init) top
        , UrlParser.map (AboutPage <| extractModel About.init) (s "about")
        , UrlParser.map (NotFoundPage <| extractModel NotFound.init) (s "not-found")
        ]



-- Esta función es clave para aplicar la seguridad y las
-- guardas de la aplicación. Acá se puede procesar
-- con el estado global de la aplicación cada página.


pageFromUrl : Url.Url -> Page
pageFromUrl url =
    case UrlParser.parse pageParser url of
        Just page ->
            page

        Nothing ->
            NotFoundPage <| extractModel NotFound.init



-- MODEL
-- ¿Acá se podria poner un estado global?


type alias Model =
    { key : Nav.Key
    , page : Page
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , page = pageFromUrl url
      }
    , Cmd.none
    )



-- UPDATE
-- Acá se conecta las páginas con su
-- modelo de mensajes y se define dos
-- tipos para el manejo de las rutas.


type Msg
    = LinkClicked Browser.UrlRequest
    | PathChanged Url.Url
    | NotFoundMsg NotFound.Msg
    | AboutMsg About.Msg
    | HomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Se ejecuta desde un link <a href="/path"></a>
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model | page = pageFromUrl url }
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        -- Se ejecuta desde la barra de navegación
        PathChanged url ->
            ( { model | page = pageFromUrl url }, Cmd.none )

        -- Cada página contine su propio update,
        -- acá se conecta con el modelo global.
        -- Cuando se emite sobe cada página se proyecta
        -- el contenido respectio de esta con su MUV.
        NotFoundMsg localMsg ->
            case model.page of
                NotFoundPage localModel ->
                    stepNotFound model (NotFound.update localMsg localModel)

                _ ->
                    ( model, Cmd.none )

        AboutMsg localMsg ->
            case model.page of
                AboutPage localModel ->
                    stepAbout model (About.update localMsg localModel)

                _ ->
                    ( model, Cmd.none )

        HomeMsg localMsg ->
            case model.page of
                HomePage localModel ->
                    stepHome model (Home.update localMsg localModel)

                _ ->
                    ( model, Cmd.none )



-- Por la anterior razón se pone las siguientes
-- funciones con un mecanismo para habilitar la
-- página conectando asi sus comandos y su modelo.


stepHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
stepHome globalModel ( localModel, localCmd ) =
    ( { globalModel | page = HomePage localModel }
    , Cmd.map HomeMsg localCmd
    )


stepAbout : Model -> ( About.Model, Cmd About.Msg ) -> ( Model, Cmd Msg )
stepAbout globalModel ( localModel, localCmd ) =
    ( { globalModel | page = AboutPage localModel }
    , Cmd.map AboutMsg localCmd
    )


stepNotFound : Model -> ( NotFound.Model, Cmd NotFound.Msg ) -> ( Model, Cmd Msg )
stepNotFound globalModel ( localModel, localCmd ) =
    ( { globalModel | page = NotFoundPage localModel }
    , Cmd.map NotFoundMsg localCmd
    )



-- VIEW
-- Acá y en las anterires funciones se usa
-- las funciones map para proyectar de lo
-- local a lo global.


view : Model -> Browser.Document Msg
view model =
    { title = "ELM | Big structure"
    , body =
        [ case model.page of
            HomePage localModel ->
                Html.map (\localeMsg -> HomeMsg localeMsg) (Home.view localModel)

            AboutPage localModel ->
                Html.map (\localeMsg -> AboutMsg localeMsg) (About.view localModel)

            NotFoundPage localModel ->
                Html.map (\localeMsg -> NotFoundMsg localeMsg) (NotFound.view localModel)
        ]
    }
