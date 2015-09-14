# erl_graph

## Intro
erl_graph is an application which will connect a d3js force directed graph to an erlang VM.
Processes will be displayed as nodes and processes which are linked to other processes will
be represented by edges.

## Usage
Add the application to your dependencies.
Then start the app with erl_graph:start().

Connect your web browser to http://127.0.0.1:8888 to see the erlang VM.
Hitting "Clear" on the web browser will clear the canvas, however all existing processes 
can be reloaded by reloading the web page.



