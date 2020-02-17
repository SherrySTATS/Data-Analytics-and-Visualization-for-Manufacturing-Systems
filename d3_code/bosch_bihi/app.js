'use strict';

var svg, tooltip, biHiSankey, path, defs, colorScale, highlightColorScale, isTransitioning;

var OPACITY = {
    NODE_DEFAULT: 0.9,
    NODE_FADED: 0.1,
    NODE_HIGHLIGHT: 0.8,
    LINK_DEFAULT: 0.6,
    LINK_FADED: 0.05,
    LINK_HIGHLIGHT: 0.9
  },

  TYPES = ["start", "good", "bad", "group1", "group2","group3", "group4"],
  TYPE_COLORS = ["#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d"],
  TYPE_HIGHLIGHT_COLORS = ["#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494"],
  LINK_COLOR = "#b3b3b3",
  INFLOW_COLOR = "#2E86D1",
  OUTFLOW_COLOR = "#D63028",
  NODE_WIDTH = 28,
  COLLAPSER = {
    RADIUS: NODE_WIDTH / 2,
    SPACING: 2
  },
  OUTER_MARGIN = 10,
  MARGIN = {
    TOP: 2 * (COLLAPSER.RADIUS + OUTER_MARGIN),
    RIGHT: OUTER_MARGIN,
    BOTTOM: OUTER_MARGIN,
    LEFT: OUTER_MARGIN
  },
  TRANSITION_DURATION = 400,
  HEIGHT = 500 - MARGIN.TOP - MARGIN.BOTTOM,
  WIDTH = 1200 - MARGIN.LEFT - MARGIN.RIGHT,
  LAYOUT_INTERATIONS = 32,
  REFRESH_INTERVAL = 7000;

var formatNumber = function (d) {
  var numberFormat = d3.format(",.0f"); // zero decimal places
  return "product quantity: " + numberFormat(d);
},

formatFlow = function (d) {
  var flowFormat = d3.format(",.0f"); // zero decimal places with sign
  return "£" + flowFormat(Math.abs(d)) + (d < 0 ? " CR" : " DR");
},

// Used when temporarily disabling user interractions to allow animations to complete
disableUserInterractions = function (time) {
  isTransitioning = true;
  setTimeout(function(){
    isTransitioning = false;
  }, time);
},

hideTooltip = function () {
  return tooltip.transition()
    .duration(TRANSITION_DURATION)
    .style("opacity", 0);
},

showTooltip = function () {
  return tooltip
    .style("left", d3.event.pageX + "px")
    .style("top", d3.event.pageY + 15 + "px")
    .transition()
      .duration(TRANSITION_DURATION)
      .style("opacity", 1);
};

colorScale = d3.scale.ordinal().domain(TYPES).range(TYPE_COLORS),
highlightColorScale = d3.scale.ordinal().domain(TYPES).range(TYPE_HIGHLIGHT_COLORS);


var svgbar = d3.select("#display").append("svg")
	.attr("id", "disp_left")
	.attr("width", 500)
	.attr("height", 300);

//Correlation map
svgbar
.append("rect")
.attr("id", "left")
.attr("x", "0")
.attr("y", "0")
.attr("width", 500)
.attr("height", 300)
.attr("fill", "url(#img)")

svgbar.append("defs")
     .append('pattern')
     .attr('id', 'img')
     .attr('patternUnits', 'userSpaceOnUse')
     .attr('width', 500)
     .attr('height', 300)
     .append("image")
     .attr("xlink:href", "cor_plot/section-1.png")
     .attr('width', 500)
     .attr('height', 300)

var data = ["section-1", "section-2", "section-3", "section-4", "section-5", "section-6",
"section-7", "section-8", "section-9","section-10", "section-11", "section-12",
"section-13", "section-14", "section-15","section-16", "section-17", "section-18",
"section-19", "section-20", "section-21","section-22", "section-23", "section-24",
"section-25", "section-26", "section-27","section-28", "section-29", "section-30",
"section-31", "section-32", "section-33","section-34", "section-35", "section-36",
"section-37", "section-38", "section-39","section-40", "section-41", "section-42",
"section-43", "section-44", "section-45","section-46", "section-47", "section-48",
"section-49", "section-50", "section-51","section-52"
];

var select = d3.select('body')
  .append('select')
  	.attr('class','select')
    .on('change',onchange)

var options = select
  .selectAll('option')
	.data(data).enter()
	.append('option')
		.text(function (d) { return d; });
	d3.select('body')
		.append('p')
		.text('Correlation map between features within a section\u00A0\u00A0\u00A0\u00A0'
		+'\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0'
		+'\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0'+
		'Real time section failure rate')
		

function onchange() {
var selectValue = d3.select('select').property('value')
svgbar
.append("rect")
.attr("id", "left")
.attr("x", "0")
.attr("y", "0")
.attr("width", 500)
.attr("height", 300)
.attr("fill", "url(#img"+selectValue+")")		
svgbar.append("defs")
     .append('pattern')
     .attr('id', 'img'+selectValue)
     .attr('patternUnits', 'userSpaceOnUse')
     .attr('width', 500)
     .attr('height', 300)
     .append("image")
     .attr("xlink:href", "cor_plot/" + selectValue + ".png")
     .attr('width', 500)
     .attr('height', 300)
		
};
    

//Production line graph
svg = d3.select("#chart").append("svg")
        .attr("width", WIDTH + MARGIN.LEFT + MARGIN.RIGHT)
        .attr("height", HEIGHT + MARGIN.TOP + MARGIN.BOTTOM)
      .append("g")
        .attr("transform", "translate(" + MARGIN.LEFT + "," + MARGIN.TOP + ")");

svg.append("defs")
     .append('pattern')
     .attr('id', 'bg')
     .attr('patternUnits', 'userSpaceOnUse')
     .attr('width', WIDTH)
     .attr('height', HEIGHT)
     .append("image")
     .attr("xlink:href", "background.png")
     .attr('width', WIDTH)
     .attr('height', HEIGHT)
     .style("opacity", 0.2);
     
svg .append("rect")
	.attr("x", "0")
	.attr("y", "0")
    .attr("width", WIDTH)
    .attr("height", HEIGHT)
    .attr("fill", "url(#bg)");

svg.append("g").attr("id", "links");
svg.append("g").attr("id", "nodes");
svg.append("g").attr("id", "collapsers");



tooltip = d3.select("#chart").append("div").attr("id", "tooltip");

tooltip.style("opacity", 0)
    .append("p")
      .attr("class", "value");
      
var flag;      
d3.json("mainData.json", function(error, initData) {
		if (error) throw error;
		
	flag = initData;
	biHiSankey = d3.biHiSankey();

	// Set the biHiSankey diagram properties
	biHiSankey
	  .nodeWidth(NODE_WIDTH)
	  .nodeSpacing(10)
	  .linkSpacing(4)
	  .arrowheadScaleFactor(0.5) 
	  .size([WIDTH, HEIGHT]);

	path = biHiSankey.link().curvature(0.45);

	defs = svg.append("defs");

	defs.append("marker")
	  .style("fill", LINK_COLOR)
	  .attr("id", "arrowHead")
	  .attr("viewBox", "0 0 6 10")
	  .attr("refX", "1")
	  .attr("refY", "5")
	  .attr("markerUnits", "strokeWidth")
	  .attr("markerWidth", "1")
	  .attr("markerHeight", "1")
	  .attr("orient", "auto")
	  .append("path")
		.attr("d", "M 0 0 L 1 0 L 6 5 L 1 10 L 0 10 z");

	defs.append("marker")
	  .style("fill", OUTFLOW_COLOR)
	  .attr("id", "arrowHeadInflow")
	  .attr("viewBox", "0 0 6 10")
	  .attr("refX", "1")
	  .attr("refY", "5")
	  .attr("markerUnits", "strokeWidth")
	  .attr("markerWidth", "1")
	  .attr("markerHeight", "1")
	  .attr("orient", "auto")
	  .append("path")
		.attr("d", "M 0 0 L 1 0 L 6 5 L 1 10 L 0 10 z");

	defs.append("marker")
	  .style("fill", INFLOW_COLOR)
	  .attr("id", "arrowHeadOutlow")
	  .attr("viewBox", "0 0 6 10")
	  .attr("refX", "1")
	  .attr("refY", "5")
	  .attr("markerUnits", "strokeWidth")
	  .attr("markerWidth", "1")
	  .attr("markerHeight", "1")
	  .attr("orient", "auto")
	  .append("path")
		.attr("d", "M 0 0 L 1 0 L 6 5 L 1 10 L 0 10 z");

	function update () {
	  var link, linkEnter, node, nodeEnter, collapser, collapserEnter;

	  function dragmove(node) {
		node.x = Math.max(0, Math.min(WIDTH - node.width, d3.event.x));
		node.y = Math.max(0, Math.min(HEIGHT - node.height, d3.event.y));
		d3.select(this).attr("transform", "translate(" + node.x + "," + node.y + ")");
		biHiSankey.relayout();
		svg.selectAll(".node").selectAll("rect").attr("height", function (d) { return d.height; });
		link.attr("d", path);
	  }

	  function containChildren(node) {
		node.children.forEach(function (child) {
		  child.state = "contained";
		  child.parent = this;
		  child._parent = null;
		  containChildren(child);
		}, node);
	  }

	  function expand(node) {
		node.state = "expanded";
		node.children.forEach(function (child) {
		  child.state = "collapsed";
		  child._parent = this;
		  child.parent = null;
		  containChildren(child);
		}, node);
	  }

	  function collapse(node) {
		node.state = "collapsed";
		containChildren(node);
	  }

	  function restoreLinksAndNodes() {
		link
		  .style("stroke", LINK_COLOR)
		  .style("marker-end", function () { return 'url(#arrowHead)'; })
		  .transition()
			.duration(TRANSITION_DURATION)
			.style("opacity", OPACITY.LINK_DEFAULT);

		node
		  .selectAll("rect")
			.style("fill", function (d) {
			  d.color = colorScale(d.type.replace(/ .*/, ""));
			  return d.color;
			})
			.style("stroke", function (d) {
			  return d3.rgb(colorScale(d.type.replace(/ .*/, ""))).darker(0.1);
			})
			.style("fill-opacity", OPACITY.NODE_DEFAULT);

		node.filter(function (n) { return n.state === "collapsed"; })
		  .transition()
			.duration(TRANSITION_DURATION)
			.style("opacity", OPACITY.NODE_DEFAULT);
	  }

	  function showHideChildren(node) {
		disableUserInterractions(2 * TRANSITION_DURATION);
		hideTooltip();
		if (node.state === "collapsed") { expand(node); }
		else { collapse(node); }

		biHiSankey.relayout();
		update();
		link.attr("d", path);
		restoreLinksAndNodes();
	  }

	  function highlightConnected(g) {
		link.filter(function (d) { return d.source === g; })
		  .style("marker-end", function () { return 'url(#arrowHeadInflow)'; })
		  .style("stroke", OUTFLOW_COLOR)
		  .style("opacity", OPACITY.LINK_DEFAULT);

		link.filter(function (d) { return d.target === g; })
		  .style("marker-end", function () { return 'url(#arrowHeadOutlow)'; })
		  .style("stroke", INFLOW_COLOR)
		  .style("opacity", OPACITY.LINK_DEFAULT);
	  }

	  function fadeUnconnected(g) {
		link.filter(function (d) { return d.source !== g && d.target !== g; })
		  .style("marker-end", function () { return 'url(#arrowHead)'; })
		  .transition()
			.duration(TRANSITION_DURATION)
			.style("opacity", OPACITY.LINK_FADED);

		node.filter(function (d) {
		  return (d.name === g.name) ? false : !biHiSankey.connected(d, g);
		}).transition()
		  .duration(TRANSITION_DURATION)
		  .style("opacity", OPACITY.NODE_FADED);
	  }




	  node = svg.select("#nodes").selectAll(".node")
		  .data(biHiSankey.collapsedNodes(), function (d) { return d.id; });

	
	  node.transition()
		.duration(TRANSITION_DURATION)
	
	
		.select("rect")
		  .style("fill", function (d) {
			d.color = colorScale(d.type.replace(/ .*/, ""));
			return d.color;
		  })
		  .style("stroke", function (d) { return d3.rgb(colorScale(d.type.replace(/ .*/, ""))).darker(0.1); })
		  .style("stroke-WIDTH", "1px")
		  .attr("height", function (d) { return d.height; })
		  .attr("width", biHiSankey.nodeWidth())



	  node.exit()
//		.transition()
	//	  .duration(TRANSITION_DURATION)
		//  .attr("transform", function (d) {
//			var collapsedAncestor, endX, endY;
//			collapsedAncestor = d.ancestors.filter(function (a) {
//			  return a.state === "collapsed";
//			})[0];
//			endX = collapsedAncestor ? collapsedAncestor.x : d.x;
//			endY = collapsedAncestor ? collapsedAncestor.y : d.y;
//			return "translate(" + endX + "," + endY + ")";
//		  })
		  .remove();


	  nodeEnter = node.enter().append("g").attr("class", "node");

	  nodeEnter

		.transition()
		  .duration(TRANSITION_DURATION)
		  .style("opacity", OPACITY.NODE_DEFAULT)
		  .attr("transform", function (d) { 
		  if (d.name == 'Production Line-1')
		  {
			  d.x = 200;
			  d.y = 200;
			  return "translate(" + d.x + "," + d.y + ")" 
		  }
		  if (d.name == 'Production Line-2')
		  {
			  d.x = 500;
			  d.y = 100;
			  return "translate(" + d.x + "," + d.y + ")" 
		  }		
		  if (d.name == 'Production Line-3')
		  {
			  d.x = 500;
			  d.y = 300;
			  return "translate(" + d.x + "," + d.y + ")" 
		  }
		  if (d.name == 'Production Line-4')
		  {
			  d.x = 800;
			  d.y = 200;
			  return "translate(" + d.x + "," + d.y + ")" 
		  }	
		  if (d.name == 'Good')
		  {
			  d.x = 950;
			  d.y = 100;
			  return "translate(" + d.x + "," + d.y + ")" 
		  }		
		  if (d.name == 'Bad')
		  {
			  d.x = 950;
			  d.y = 300;
			  return "translate(" + d.x + "," + d.y + ")" 
		  }			  

		  if (d.name == 'S_29')
		  {
			  d.x = 600;
			  d.y = 150;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }
		  if (d.name == 'S_30')
		  {
			  d.x = 600;
			  d.y = 200;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_31')
		  {
			  d.x = 600;
			  d.y = 290;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }
		  if (d.name == 'S_32')
		  {
			  d.x = 680;
			  d.y = 10;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_33')
		  {
			  d.x = 680;
			  d.y = 60;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_34')
		  {
			  d.x = 680;
			  d.y = 110;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }			
		  if (d.name == 'S_35')
		  {
			  d.x = 680;
			  d.y = 300;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }		
		  if (d.name == 'S_36')
		  {
			  d.x = 680;
			  d.y = 340;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_37')
		  {
			  d.x = 680;
			  d.y = 380;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_38')
		  {
			  d.x = 750;
			  d.y = 110;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_39')
		  {
			  d.x = 750;
			  d.y = 190;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_40')
		  {
			  d.x = 750;
			  d.y = 250;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }		
		  if (d.name == 'S_41')
		  {
			  d.x = 750;
			  d.y = 300;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_43')
		  {
			  d.x = 850;
			  d.y = 10;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }		
		  if (d.name == 'S_44')
		  {
			  d.x = 850;
			  d.y = 40;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_45')
		  {
			  d.x = 850;
			  d.y = 70;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_47')
		  {
			  d.x = 850;
			  d.y = 100;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_48')
		  {
			  d.x = 850;
			  d.y = 300;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_49')
		  {
			  d.x = 850;
			  d.y = 330;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }		
		  if (d.name == 'S_50')
		  {
			  d.x = 850;
			  d.y = 360;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }		
		  if (d.name == 'S_51')
		  {
			  d.x = 850;
			  d.y = 390;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_24')
		  {
			  d.x = 420;
			  d.y = 30;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_25')
		  {
			  d.x = 420;
			  d.y = 100;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_26')
		  {
			  d.x = 420;
			  d.y = 250;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_27')
		  {
			  d.x = 420;
			  d.y = 300;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }	
		  if (d.name == 'S_28')
		  {
			  d.x = 420;
			  d.y = 360;
			  return "translate(" + d.x + "," + d.y + ")" 			  
		  }		
	      if (d.name == 'S_0')
          {
              d.x = 50;
              d.y = 100;
              return "translate(" + d.x + "," + d.y + ")"
          }		  
          if (d.name == 'S_1')
          {
              d.x = 50;
              d.y = 150;
              return "translate(" + d.x + "," + d.y + ")"
          }
          if (d.name == 'S_2')
          {
              d.x = 50;
              d.y = 200;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_3')
          {
              d.x = 50;
              d.y = 300;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_4')
          {
              d.x = 120;
              d.y = 10;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_5')
          {
              d.x = 120;
              d.y = 50;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_6')
          {
              d.x = 120;
              d.y = 80;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_7')
          {
              d.x = 120;
              d.y = 120;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_8')
          {
              d.x = 120;
              d.y = 220;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_9')
          {
              d.x = 120;
              d.y = 250;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_10')
          {
              d.x = 120;
              d.y = 320;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_11')
          {
              d.x = 120;
              d.y = 400;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_12')
          {
              d.x = 190;
              d.y = 10;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_13')
      {
        d.x = 190;
        d.y = 50;
        return "translate(" + d.x + "," + d.y + ")"
      }
      if (d.name == 'S_14')
          {
              d.x = 190;
              d.y = 80;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_15')
          {
              d.x = 190;
              d.y = 120;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_16')
          {
              d.x = 190;
              d.y = 220;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_17')
          {
              d.x = 190;
              d.y = 250;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_18')
          {
              d.x = 190;
              d.y = 300;
              return "translate(" + d.x + "," + d.y + ")"
          }
      if (d.name == 'S_19')
          {
              d.x = 190;
              d.y = 370;
              return "translate(" + d.x + "," + d.y + ")"
          }

      if (d.name == 'S_20')
      {
        d.x = 260;
        d.y = 150;
        return "translate(" + d.x + "," + d.y + ")"
      }
      if (d.name == 'S_21')
      {
        d.x = 260;
        d.y = 210;
        return "translate(" + d.x + "," + d.y + ")"
      }
      if (d.name == 'S_22')
      {
        d.x = 260;
        d.y = 250;
        return "translate(" + d.x + "," + d.y + ")"
      }
      if (d.name == 'S_23')
      {
        d.x = 260;
        d.y = 330;
        return "translate(" + d.x + "," + d.y + ")"
      }		  
		  else
		  return "translate(" + d.x + "," + d.y + ")"; }); //transition node location

	  nodeEnter.append("text");
	  nodeEnter.append("rect")
		.style("fill", function (d) {
		  d.color = colorScale(d.type.replace(/ .*/, ""));
		  return d.color;
		})
		.style("stroke", function (d) {
		  return d3.rgb(colorScale(d.type.replace(/ .*/, ""))).darker(0.1);
		})
		.style("stroke-WIDTH", "1px")
		.attr("height", function (d) { return d.height; })
		.attr("width", biHiSankey.nodeWidth());

	  node.on("mouseenter", function (g) {
		if (!isTransitioning) {
		  restoreLinksAndNodes();
		  highlightConnected(g);
		  fadeUnconnected(g);

		  d3.select(this).select("rect")
			.style("fill", function (d) {
			  d.color = d.netFlow > 0 ? INFLOW_COLOR : OUTFLOW_COLOR;
			  return d.color;
			})
			.style("stroke", function (d) {
			  return d3.rgb(d.color).darker(0.1);
			})
			.style("fill-opacity", OPACITY.LINK_DEFAULT);

		  tooltip
			.style("left", g.x + MARGIN.LEFT + "px")
			.style("top", g.y + g.height + MARGIN.TOP + 15 + "px")
			.transition()
			  .duration(TRANSITION_DURATION)
			  .style("opacity", 1).select(".value")
			  .text(function () {
				var additionalInstructions = g.children.length ? "\n(Double click to expand)" : "";
	//             return g.name + "\nNet flow: " + formatFlow(g.netFlow) + additionalInstructions;
				return g.name + "\nFailure Rate: " + g.Frate + additionalInstructions;
			  });
		}
	  });

	  node.on("mouseleave", function () {
		if (!isTransitioning) {
		  hideTooltip();
		  restoreLinksAndNodes();
		}
	  });
	  link = svg.select("#links").selectAll("path.link")
		.data(biHiSankey.visibleLinks(), function (d) { return d.id; });

	  link.transition()
		.duration(TRANSITION_DURATION)
		.style("stroke-WIDTH", function (d) { return Math.max(1, d.thickness); })
		.attr("d", path)
		.style("opacity", OPACITY.LINK_DEFAULT);


	  link.exit().remove();


	  linkEnter = link.enter().append("path")
		.attr("class", "link")
		.style("fill", "none");

	  linkEnter.on('mouseenter', function (d) {
		if (!isTransitioning) {
		  showTooltip().select(".value").text(function () {
			if (d.direction > 0) {
			  return d.source.name + " → " + d.target.name + "\n" + formatNumber(d.value);
			}
			return d.target.name + " ← " + d.source.name + "\n" + formatNumber(d.value);
		  });

		  d3.select(this)
			.style("stroke", LINK_COLOR)
			.transition()
			  .duration(TRANSITION_DURATION / 2)
			  .style("opacity", OPACITY.LINK_HIGHLIGHT);
		}
	  });

	  linkEnter.on('mouseleave', function () {
		if (!isTransitioning) {
		  hideTooltip();

		  d3.select(this)
			.style("stroke", LINK_COLOR)
			.transition()
			  .duration(TRANSITION_DURATION / 2)
			  .style("opacity", OPACITY.LINK_DEFAULT);
		}
	  });

	  linkEnter.sort(function (a, b) { return b.thickness - a.thickness; })
		.classed("leftToRight", function (d) {
		  return d.direction > 0;
		})
		.classed("rightToLeft", function (d) {
		  return d.direction < 0;
		})
		.style("marker-end", function () {
		  return 'url(#arrowHead)';
		})
		.style("stroke", LINK_COLOR)
		.style("opacity", 0)
		.transition()
		  .delay(TRANSITION_DURATION)
		  .duration(TRANSITION_DURATION)
		  .attr("d", path)
		  .style("stroke-WIDTH", function (d) { return Math.max(1, d.thickness); })
		  .style("opacity", OPACITY.LINK_DEFAULT);
	  /**
	  * Fix to allow for dblclick on dragging element
	  * This essentially checks to see if the vectors are in the same location once the drag
	  * has ended.
	  */

	  var lastvector = []
	  function isclicked(node){
		try {
		  if( lastvector[node.id].toString() !== [node.x,node.y].toString() ){
			throw 'no match';
		  }
		  showHideChildren(node);
		}catch(err) {
		  lastvector[node.id] = [node.x,node.y]
		}
	  }

	  // allow nodes to be dragged to new positions
	  node.call(d3.behavior.drag()
		.origin(function (d) { return d; })
		.on("dragstart", function () { node.event,this.parentNode.appendChild(this); })
		.on("dragend", isclicked)
		.on("drag", dragmove));

	  // add in the text for the nodes
	  node.filter(function (d) { return d.value !== 0; })
		.select("text")
		  .attr("x", -6)
		  .attr("y", function (d) { return d.height / 2; })
		  .attr("dy", ".35em")
		  .attr("text-anchor", "end")
		  .attr("transform", null)
		  .text(function (d) { return d.name; })
		.filter(function (d) { return d.x < WIDTH / 2; })
		  .attr("x", 6 + biHiSankey.nodeWidth())
		  .attr("text-anchor", "start");


	  collapser = svg.select("#collapsers").selectAll(".collapser")
		.data(biHiSankey.expandedNodes(), function (d) { return d.id; });


	  collapserEnter = collapser.enter().append("g").attr("class", "collapser");

	  collapserEnter.append("circle")
		.attr("r", COLLAPSER.RADIUS)
		.style("fill", function (d) {
		  d.color = colorScale(d.type.replace(/ .*/, ""));
		  return d.color;
		});

	  collapserEnter
		.style("opacity", OPACITY.NODE_DEFAULT)
		.attr("transform", function (d) {
		  return "translate(" + (d.x + d.width / 2) + "," + (d.y + COLLAPSER.RADIUS) + ")";
		});

	  collapserEnter.on("dblclick", showHideChildren);

	  collapser.select("circle")
		.attr("r", COLLAPSER.RADIUS);

	  collapser.transition()
		.delay(TRANSITION_DURATION)
		.duration(TRANSITION_DURATION)
		.attr("transform", function (d, i) {
		  return "translate("
			+ (COLLAPSER.RADIUS + i * 2 * (COLLAPSER.RADIUS + COLLAPSER.SPACING))
			+ ","
			+ (-COLLAPSER.RADIUS - OUTER_MARGIN)
			+ ")";
		});

	  collapser.on("mouseenter", function (g) {
		if (!isTransitioning) {
		  showTooltip().select(".value")
			.text(function () {
			  return g.name + "\n(Double click to collapse)";
			});

		  var highlightColor = highlightColorScale(g.type.replace(/ .*/, ""));

		  d3.select(this)
			.style("opacity", OPACITY.NODE_HIGHLIGHT)
			.select("circle")
			  .style("fill", highlightColor);

		  node.filter(function (d) {
			return d.ancestors.indexOf(g) >= 0;
		  }).style("opacity", OPACITY.NODE_HIGHLIGHT)
			.select("rect")
			  .style("fill", highlightColor);
		}
	  });

	  collapser.on("mouseleave", function (g) {
		if (!isTransitioning) {
		  hideTooltip();
		  d3.select(this)
			.style("opacity", OPACITY.NODE_DEFAULT)
			.select("circle")
			  .style("fill", function (d) { return d.color; });

		  node.filter(function (d) {
			return d.ancestors.indexOf(g) >= 0;
		  }).style("opacity", OPACITY.NODE_DEFAULT)
			.select("rect")
			  .style("fill", function (d) { return d.color; });
		}
	  });

	  collapser.exit().remove();

	}



	biHiSankey
	  .nodes(initData.nodes)
	  .links(initData.links)
	  .initializeNodes(function (node) {
		node.state = node.parent ? "contained" : "collapsed";
	  })
	  .layout(LAYOUT_INTERATIONS);

	disableUserInterractions(2 * TRANSITION_DURATION);

	update();
});