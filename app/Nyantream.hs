module Nyantream where

import Client
import Plugins.Twitter
import Plugins.Timer

installedPlugins :: [Plugin]
installedPlugins = [twitterM "myuon_myon", twitterM "u_inai", hscheduler]

main = runClient installedPlugins

