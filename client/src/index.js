import * as d3 from 'd3';

import './main.css';
import {Main} from './Main.elm';

const app = Main.embed(document.getElementById('root'));

const width = 1000;
const height = 800;

const center = {x: width / 2, y: height / 2};

const absent_or_both_position = {x: width / 2, y: height / 2};
const positions = {
  yes: {x: width / 3, y: height / 2},
  absent: absent_or_both_position,
  both: absent_or_both_position,
  no: {x: 2 * width / 3, y: height / 2},
};

const forceStrength = 0.03;

const bubbleChart = (() => {
  let bubbles;
  let nodes;

  // Charge function that is called for each node.  As part of the ManyBody
  // force. This is what creates the repulsion between nodes.
  //
  // Charge is proportional to the diameter of the circle (which is stored in
  // the radius attribute of the circle's associated data.
  //
  // This is done to allow for accurate collision detection with nodes of
  // different sizes.
  //
  // Charge is negative because we want nodes to repel.
  function charge(d) {
    return -Math.pow(d.radius, 2.0) * forceStrength;
  }

  const simulation = d3
    .forceSimulation()
    .velocityDecay(0.2)
    .force(
      'x',
      d3
        .forceX()
        .strength(forceStrength)
        .x(optionPosition),
    )
    .force(
      'y',
      d3
        .forceY()
        .strength(forceStrength)
        .y(center.y),
    )
    .force('charge', d3.forceManyBody().strength(charge))
    .on('tick', ticked);

  // Force simulation starts automatically, which we don't want as there aren't
  // any nodes yet.
  simulation.stop();

  function createNodes(rawData) {
    const myNodes = rawData.map(function(d) {
      return {
        id: d.id,
        radius: 10,
        colour: d.partyColour,
        option: d.option,
        x: Math.random() * 900,
        y: Math.random() * 800,
      };
    });

    return myNodes;
  }

  const chart = function chart(selector, rawData) {
    nodes = createNodes(rawData);

    // Create a SVG element inside the provided selector with desired size.
    const svg = d3
      .select(selector)
      .append('svg')
      .attr('width', width)
      .attr('height', height);

    // Bind nodes data to what will become DOM elements to represent them.
    bubbles = svg.selectAll('.bubble').data(nodes, d => d.id);

    // Create new circle elements each with class `bubble`.  There will be one
    // circle.bubble for each object in the nodes array.  Initially, their
    // radius (r attribute) will be 0.  Selections are immutable, so lets
    // capture the enter selection to apply our transition to below.
    const bubblesE = bubbles
      .enter()
      .append('circle')
      .classed('bubble', true)
      .attr('r', 0)
      .attr('fill', function(d) {
        return d.colour;
      })
      .attr('stroke', function(d) {
        return d3.rgb(d.colour).darker();
      })
      .attr('stroke-width', 2);
    // .on('mouseover', showDetail)
    // .on('mouseout', hideDetail);

    // Merge the original empty selection and the enter selection
    bubbles = bubbles.merge(bubblesE);

    // Fancy transition to make bubbles appear, ending with the correct radius
    bubbles
      .transition()
      .duration(2000)
      .attr('r', function(d) {
        return d.radius;
      });

    // Set the simulation's nodes to our newly created nodes array.
    simulation.nodes(nodes);

    // Reset the alpha value and restart the simulation
    simulation.alpha(1).restart();
  };

  // Called after every tick of the force simulation. Here we do the actual
  // repositioning of the SVG circles based on the current x and y values of
  // their bound node data.  These x and y values are modified by the force
  // simulation.
  function ticked() {
    bubbles
      .attr('cx', function(d) {
        return d.x;
      })
      .attr('cy', function(d) {
        return d.y;
      });
  }

  function optionPosition(d) {
    return positions[d.option].x;
  }

  return chart;
})();

app.ports.graphData.subscribe(data => bubbleChart('#d3-simulation', data));
