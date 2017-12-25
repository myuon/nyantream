module Nyantream where

import Client
import Types

import Plugins.Twitter
import Plugins.Timer
import Plugins.Gmail

installedPlugins =
  [ twitter "myuon_myon"
  , twitter "u_inai"
  , gmail "ioijoikoiloi"
  , hscheduler
  ]

main = runClient installedPlugins

