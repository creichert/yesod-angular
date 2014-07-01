{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Yesod Angular JS Integration.
--
--   This module is based on Michael Snoyman's original work
--   in the <https://github.com/snoyberg/yesod-js> repository.
--
--   * This module currently defaults to Angular 1.2.18. Use
--     `urlAngularJs_` specify your own Angular location.
--   * Example can be found at:
--        <https://github.com/snoyberg/yesod-js/tree/master/yesod-angular>
--   * Currently, this module looks for controllers in the
--     `templates/angular` directory.
module Yesod.Angular
    ( YesodAngular (..)
    , runAngular
    , addCommand
    , addCtrl
    , addCtrlRaw
    , setDefaultRoute
    , AngularT
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad.Trans.Writer (WriterT(..), tell)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Char (isAlpha)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid, First(..), (<>), mempty, mappend)
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.Haskell.TH.Syntax (Q, Exp (AppE, LitE),
                                             Lit (StringL))
import           Text.Hamlet (hamletFile)
import           Text.Julius (JavascriptUrl, juliusFile, rawJS)
import           Yesod


-- | YesodAngular wraps a widget in ng-app named @modname.
class Yesod site => YesodAngular site where
    -- | Default instance loads `angular.min.js` and `angular-route.min.js`.
    urlAngularJs :: site -> [Either (Route site) Text]
    urlAngularJs _ = [Right "//cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.18/angular.min.js",
                      Right "//cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.18/angular-route.min.js"]

    wrapAngular :: Text -> WidgetT site IO () -> HandlerT site IO Html
    wrapAngular modname widget = defaultLayout [whamlet| <div ng-app=#{modname}>^{widget} |]


data AngularWriter site = AngularWriter
    { awCommands     :: Map Text (HandlerT site IO ())
    , awPartials     :: Map Text (HtmlUrl (Route site))
    , awRoutes       :: JavascriptUrl (Route site)
    , awControllers  :: JavascriptUrl (Route site)
    , awDefaultRoute :: First Text
    }


type AngularT site = WriterT (AngularWriter site) (HandlerT site IO)


instance Monoid (AngularWriter site) where
    mempty = AngularWriter mempty mempty mempty mempty mempty
    AngularWriter a1 a2 a3 a4 a5
        `mappend` AngularWriter b1 b2 b3 b4 b5 = AngularWriter
                                                    (mappend a1 b1)
                                                    (mappend a2 b2)
                                                    (mappend a3 b3)
                                                    (mappend a4 b4)
                                                    (mappend a5 b5)


runAngular :: YesodAngular site
           => AngularT site ()
           -> HandlerT site IO Html
runAngular ga = do
    site <- getYesod
    ((), AngularWriter{..}) <- runWriterT ga
    mc <- lookupGetParam "command"
    fromMaybe (return ()) $ mc >>= flip Map.lookup awCommands
    mp <- lookupGetParam "partial"
    case mp >>= flip Map.lookup awPartials of
        Nothing -> return ()
        Just htmlurl -> do
            ps <- getUrlRenderParams
            let rep = toTypedContent . htmlurl $ ps
            sendResponse rep

    modname <- newIdent

    let defaultRoute =
            case awDefaultRoute of
                First (Just x) -> [julius|.otherwise({redirectTo:"#{rawJS x}"})|]
                First Nothing -> mempty

    wrapAngular modname $ do
        mapM_ addScriptEither $ urlAngularJs site
        [whamlet| <div ng-view> |]
        toWidget [julius|
            angular.module("#{rawJS modname}", ['ngRoute']).config(["$routeProvider",
                function($routeProvider, $locationProvider) {
                    $routeProvider ^{awRoutes} ^{defaultRoute};
                }]);
            ^{awControllers}
        |]


addCommand :: (FromJSON input, ToJSON output)
           => (input -> HandlerT site IO output)
           -> AngularT site Text
addCommand f = do
    name <- lift newIdent
    tell mempty { awCommands = Map.singleton name handler }
    return $ "?command=" `mappend` name
  where
    handler = do
        input <- requireJsonBody
        output <- f input
        repjson <- returnJson output
        sendResponse repjson


setDefaultRoute :: Text -> AngularT site ()
setDefaultRoute x = tell mempty { awDefaultRoute = First $ Just x }


addCtrl :: Text -- ^ route pattern
        -> Text -- ^ template name
        -> Q Exp
addCtrl route name = do
    let name' = T.filter isAlpha name
    [|addCtrlRaw $(liftT name') $(liftT route) $(hamletFile $ fn "hamlet") $(juliusFile $ fn "julius")|]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t
    fn suffix = T.unpack $ T.concat ["angular/", name, ".", suffix]


addCtrlRaw :: Text -- ^ user-friendly name
           -> Text -- ^ route pattern
           -> HtmlUrl (Route site) -- ^ template
           -> JavascriptUrl (Route site) -- ^ controller
           -> AngularT site ()
addCtrlRaw name' route template controller = do
    name <- mappend (mappend name' "__") <$> lift newIdent
    tell mempty
        { awPartials = Map.singleton name template
        , awRoutes = [julius| .when("#{rawJS route}",
                                { "controller": #{rawJS name}
                                , "templateUrl": "?partial=#{rawJS name}"
                                })
                     |]
        , awControllers = [julius| var #{rawJS name} = ^{controller} |]
        }
