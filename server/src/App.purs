module Pure.App where

import Prelude
import Pure.PrimarySup as Pure.PrimarySup

import Pinto.App as App

start = App.simpleStart Pure.PrimarySup.startLink
