var palette = {
      "lightgray": "#819090",
      "gray": "#708284",
      "mediumgray": "#536870",
      "darkgray": "#475B62",

      "darkblue": "#0A2933",
      "darkerblue": "#042029",

      "paleryellow": "#FCF4DC",
      "paleyellow": "#EAE3CB",
      "yellow": "#A57706",
      "orange": "#BD3613",
      "red": "#D11C24",
      "pink": "#C61C6F",
      "purple": "#595AB7",
      "blue": "#2176C7",
      "green": "#259286",
      "yellowgreen": "#738A05"
  };

function myGraph(el) {

    this.clear_data = function() {
        while(nodes.length > 0) {
            nodes.pop();
        }
        links = [];
        this.update();
    };

    this.addNode = function (process) {
        var index = findNodeIndex(process.pid);
        if (index != undefined) {
            for (var k in process) {
                nodes[index][k] = process[k];
            }
        }
        else {
            nodes.push(process);
        }
    };

    this.removeNode = function (process) {
        var i = 0;
        var n = this.findNode(process.pid);
        while (i < links.length) {
            if ((links[i]['source'] === n)||(links[i]['target'] == n)){
                links.splice(i,1);
            }
            else {
                i++;
            }
        }
        var index = findNodeIndex(process.pid);
        if(index !== undefined) {
            nodes.splice(index, 1);
        }
    };

    this.addLink = function (source_process, target_process) {
        var sourceNode = this.findNode(source_process.pid);
        var targetNode = this.findNode(target_process.pid);

        if((sourceNode !== undefined) && (targetNode !== undefined)) {
            links.push({"source": sourceNode, "target": targetNode});
        }
    };

    this.findNode = function (pid) {
        for (var i=0; i < nodes.length; i++) {
            if (nodes[i].pid === pid)
                return nodes[i]
        };
    };

    var findNodeIndex = function (pid) {
        for (var i=0; i < nodes.length; i++) {
            if (nodes[i].pid === pid)
                return i
        };
    };

    var width = 2500, height = 2500;
    var vis = this.vis = d3.select(el)
        .append("svg")
        .attr("id", "svg-graph")
        .attr('width', width)
        .attr('height', height);

    var force = d3.layout.force()
        .size([width, height])
        .gravity(.05)
        .distance(100)
        .charge(-150);

    var nodes = force.nodes(),
        links = force.links();

    this.update = function () {

        var link = vis.selectAll("line.link")
            .data(links, function(d) { return d.source.pid + "-" + d.target.pid; });

        link.enter().insert("line")
            .attr('stroke', palette.gray)
            .attr("class", "link");

        link.exit().remove();

        var node = vis.selectAll("g.node")
            .data(nodes, function(d) { return d.pid;});

        vis.selectAll(".nodetext").text(function(d) {return d.registered_name});

        var nodeEnter = node.enter().append("g")
            .attr("class", "node")
            .call(force.drag);

        nodeEnter.append('circle')
            .attr('cx', function(d) { return d.x; })
            .attr('cy', function(d) { return d.y; })
            .attr('r', 10 )
            .attr('fill', palette.blue);

        nodeEnter.append("text")
            .attr("class", "nodetext")
            .attr("dx", 12)
            .attr("dy", ".35em")
            .text(function(d) {return d.registered_name});

        node.exit().remove();

        force.on("tick", function() {
          link.attr("x1", function(d) { return d.source.x; })
              .attr("y1", function(d) { return d.source.y; })
              .attr("x2", function(d) { return d.target.x; })
              .attr("y2", function(d) { return d.target.y; });

          node.attr("transform", function(d) {
              return "translate(" + d.x + "," + d.y + ")";
          });
        });
        force.start();
    };
    this.update();
}

function msgHandler() {
    this.connect = function() {
        ws = new window.WebSocket("ws://" + window.location.host + "/ws");

        ws.onclose = function(){
            setTimeout(function(){this.connect;}, 500);
        }

        ws.onmessage = function(e){
            json = JSON.parse(e.data);
            //console.log(json);
            handle_json(json);
        }
    }
}

graph = new myGraph("#graph");
msg_handler = new msgHandler();
msg_handler.connect();

clear_data = function() {graph.clear_data()};

handle_json = function(obj) {
        if (obj.action === "add_node") {
            graph.addNode(obj.process);
            for (j=0; j<obj.process.links.length; ++j) {
                child_pid = obj.process.links[j];
                child = graph.findNode(child_pid);
                if (!child) {
                    child = make_node(child_pid);
                    graph.addNode(child);
                }
                graph.addLink(obj.process, child);
            }
        }
        if (obj.action === "remove_node") {
            graph.removeNode(obj.process);
        }
        else if (obj.action === "add_link") {
            graph.addLink(obj.from.process, obj.to.process);
        }
        graph.update();
    };

function make_node(pid) {
    return {"pid": pid, "registered_name": pid};
}

