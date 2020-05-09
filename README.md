# A Simple App
This app is based on the day8/re-frame example app.

All the code is in one namespace: `/src/simple/core.cljs`.

### Run It And Change It   

Steps:

1. Run "`lein do clean, shadow watch client`"  to compile the app and start up shadow-cljs hot-reloading
2. Wait for the compile to finish. At a minumum, that might take 15 seconds. But it can take more like 60 seconds if you are new to ClojureScript and various caches are empty. Eventually you should see `[:client] Build Completed (... stats ...)`
3. Open `http://localhost:8280/index.html` to see the app

While step 1 is running, any changes you make to the ClojureScript 
source files (in `src`) will be re-compiled and reflected in the running 
page immediately.

### Production Version

Run "`lein do clean, shadow release client`" to compile an optimised 
version, and then open `resources/public/index.html` in a browser.
