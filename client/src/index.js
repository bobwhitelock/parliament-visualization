import * as d3 from 'd3';

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

class BubbleChart {
  constructor(selector) {
    this.bubbles = null;
    this.nodes = null;

    // Called after every tick of the force simulation. Here we do the actual
    // repositioning of the SVG circles based on the current x and y values of
    // their bound node data.  These x and y values are modified by the force
    // simulation.
    this.ticked = () => {
      this.bubbles.attr('cx', d => d.x).attr('cy', d => d.y);
    };

    this.simulation = d3
      .forceSimulation()
      .velocityDecay(0.2)
      .force(
        'x',
        d3
          .forceX()
          .strength(forceStrength)
          .x(this.optionPosition),
      )
      .force(
        'y',
        d3
          .forceY()
          .strength(forceStrength)
          .y(center.y),
      )
      .force('charge', d3.forceManyBody().strength(this.charge))
      .on('tick', this.ticked);

    // Force simulation starts automatically, which we don't want as there aren't
    // any nodes yet.
    this.simulation.stop();

    // Create a SVG element inside the provided selector with desired size.
    this.svg = d3
      .select(selector)
      .append('svg')
      .attr('width', width)
      .attr('height', height);
  }

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
  charge(d) {
    return -Math.pow(d.radius, 2.0) * forceStrength;
  }

  setNodes(rawData) {
    this.nodes = rawData.map(d => {
      const currentNode = ((this.nodes &&
        this.nodes.filter(node => node.personId == d.personId)) ||
        {})[0];

      return {
        personId: d.personId,
        radius: 10,
        colour: d.partyColour,
        option: d.option,
        x: currentNode ? currentNode.x : Math.random() * 900,
        y: currentNode ? currentNode.y : Math.random() * 800,
      };
    });

    // Bind nodes data to what will become DOM elements to represent them.
    this.bubbles = this.svg
      .selectAll('.bubble')
      .data(this.nodes, d => d.personId);

    this.bubbles.exit().remove();

    const personNodeHovered = node =>
      app.ports.personNodeHovered.send(node.personId);

    // Create new circle elements each with class `bubble`.  There will be one
    // circle.bubble for each object in the nodes array.  Initially, their
    // radius (r attribute) will be 0.  Selections are immutable, so lets
    // capture the enter selection to apply our transition to below.
    const bubblesE = this.bubbles
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
      .attr('stroke-width', 2)
      .on('mouseover', personNodeHovered);
    // .on('mouseout', hideDetail);

    // Merge the original empty selection and the enter selection
    this.bubbles = this.bubbles.merge(bubblesE);

    // Fancy transition to make bubbles appear, ending with the correct radius
    this.bubbles
      .transition()
      .duration(2000)
      .attr('r', function(d) {
        return d.radius;
      });

    // Set the simulation's nodes to our newly created nodes array.
    this.simulation.nodes(this.nodes);

    // Reset the alpha value and restart the simulation
    this.simulation.alpha(1).restart();
  }

  optionPosition(d) {
    return positions[d.option].x;
  }
}

let chartExistenceIntervalId = null;
let chart = null;

const renderChart = data => {
  if (!chart) {
    chart = new BubbleChart('#d3-simulation');
  }
  chart.setNodes(data);
};

const chartElementExists = () => !!document.getElementById('d3-simulation');

const createChartIfElementExists = data => () => {
  if (chartElementExists()) {
    renderChart(data);
    window.clearTimeout(chartExistenceIntervalId);
  }
};

app.ports.chartData.subscribe(data => {
  if (chartElementExists()) {
    renderChart(data);
  } else {
    chartExistenceIntervalId = window.setInterval(
      createChartIfElementExists(data),
      10,
    );
  }
});
