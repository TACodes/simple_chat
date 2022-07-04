simple_chat
=====

A simple OTP erlang chat application.
Websocket is used for real time messaging.

It checks if you are logged in and store a cookie session.
After 10 minutes the cookie session is deleted along with
the user and all its information.

Quick Start
-----

Install rebar3 and OTP-22+

Build a release in production mode as tar file
> rebar3 as prod tar

Unpack a tar.gz file to a folder and run the application
> tar -zxvf simple_chat-0.1.0.tar.gz

> ./bin/simple_chat console
