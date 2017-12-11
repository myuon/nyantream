module Nyantream where

import Client
import Types
import Plugins.Twitter
import Plugins.Timer

installedPlugins :: [Plugin]
installedPlugins = [twitter "myuon_myon", twitter "u_inai", hscheduler]

main = runClient installedPlugins

