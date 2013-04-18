Surf
====
Surf is a small, experimental webserver written in Haskell.

Security issues
---------------
Surf should not be considered safe.

In principle you should use _authbind_ or _privbind_ when setting Surf up on
port 80. I could not get this to work.

Instead Surf drops priviledges via `setuid`. That is, it switches to the user
specified in the configuration (conf.ini).

*References*
http://stackoverflow.com/questions/394984/best-practice-to-run-linux-service-as-a-different-user
http://stackoverflow.com/questions/9330367/how-to-configure-jenkins-to-run-on-port-80
http://kvz.io/blog/2009/12/15/run-nodejs-as-a-service-on-ubuntu-karmic/
http://hackage.haskell.org/trac/ghc/ticket/3910


> To bind to port 80 without using `sudo` use the package *authbind*
>     $ touch /etc/authbind/byport/80
>     $ chown www:www /etc/authbind/byport/80
>     $ chmod u+x /etc/authbind/byport/80
> 
>     $ sudo -u www authbind --deep runhaskell Surf.hs
> 
> To bind to port 80 without using `sudo` use the package *privbind*
> 
>     $ sudo privbind -u 1000 -g 1000 runhaskell Surf.hs
