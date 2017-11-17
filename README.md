# erlgraph - Visualizing the Erlang VM via a D3JS Force Directed Graph

## Intro
erlgraph is an application which will connect a [d3js](http://d3js.org) force directed graph to an Erlang VM.

The nodes in the graph represent erlang processes.  They will contain either the registered
name of the process or the process pid where there is no registered name.

Edges between the nodes will represent a relationship between the processes i.e. processes
which are linked or processes which have spawned other processes.

This application is mostly for demonstration purposes of how processes look in the Erlang VM
however it will be possible to extend it so that more detailed information about each process
can be fetched by the web interface from the VM via the websocket.


## Installing
You will need Erlang R17 as this application uses a version of cowboy which is depending on R17.


### As a standalone application
To install as standalone, type 'make'.
This will fetch all dependencies and start up an Erlang VM running erlgraph.


### As a dependency withing another application
erlgraph may be added as a dependency for your existing application and this is the usual
case as you will be able to see all the processes spawned by your application.

erlgraph may be started with: erlgraph_app:start().


## Usage
Set the port you want the webserver to run on in src/erlgraph.app.src, or add to your
app.config the following (which assumes you want to use port 8888):
{erlgraph, [
    {port, 8888}
]}

Connect your web browser to http://127.0.0.1:8888 to see the Erlang VM.

Hitting "Clear" on the web browser will clear the canvas, however all existing processes
can be reloaded by reloading your web page.
