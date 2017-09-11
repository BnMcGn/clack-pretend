## Clack-pretend: a testing and debugging tool for clack.

Are you tired of jumping to your web browser every time you need to test your work in Clack? Clack-pretend will capture and replay calls to your clack middleware stack. When developing a web application with clack, you will often find it inconvenient to run your code from the lisp REPL because it expects a clack environment, including perhaps, cookies or a logged-in user. With clack-pretend, you can run prior web requests from your REPL, moving development back where it belongs.

##Setup

First, replace lack:builder or clack:builder:

    (lack:builder
      ...

with pretend-builder:

    (clack-pretend:pretend-builder (:insert 3)
      ...

Pretend builder, aside from its recording features, is a thin wrapper around lack:builder.

The :insert parameter is where pretend will capture the environment and output. For example given this app:

    (clack-pretend:pretend-builder (:insert 3)
      (:static :path "~/public_html/")
      :accesslog
      :session
      (custom-middleware param)
      (my-app))

The collection point will be inserted just after the session middleware, and just before custom-middleware.

To use clack-pretend, restart your clack app and visit a page with your web browser.

# Quick summary

    (quick-summary)

This function gives a list of URLs that have been captured.

# Info functions

Most of the info functions hava an optional parameter named index. The default is 0, or the last request received by the stack. By default clack-pretend stores up to 10 requests. Higher indices retrieve older requests.

    (last-input (&optional (index 0))

The input environment as it was received from the middleware above the insertion point.

    (last-output (&optional (index 0))

The output of the app below the insertion point.

    (last-request-object (&optional (index 0))

A clack request object generated from the results of last-input.

    (last-request-url ())        

The URL of the last request.

    (last-session (&optional (index 0))

The session object from the last request.

    (last-as-code (&optional (index 0))

Attempts to output the last environment and session as usable source code. This is useful for turning a web request into something usable as a unit test for your middleware.

# Running your last request

    > (run-pretend)

Runs the last web request starting below the insert point with the saved environment information from above the insert point.

# Author

Ben McGunigle (bnmcgn at gmail.com)

# Copyright

Copyright (c) 2017 Ben McGunigle

# License

Apache License version 2.0
