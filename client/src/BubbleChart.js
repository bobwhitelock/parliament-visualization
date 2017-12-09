import * as d3 from 'd3';

const forceStrength = 0.03;

// Threshold where, when the simulation alpha goes below this, we will send a
// message to the Elm app to inform it that the current chart simulation is
// almost complete.
const chartSettledThreshold = 0.2;

export default class BubbleChart {
  constructor(app, selector) {
    this.app = app;
    this.svg = d3.select(selector);

    this.width = this.svg.attr('width');
    this.height = this.svg.attr('height');

    const left = {x: this.width / 3, y: this.height / 2};
    const center = {x: this.width / 2, y: this.height / 2};
    const right = {x: 2 * this.width / 3, y: this.height / 2};
    this.positions = {
      yes: left,
      absent: center,
      both: center,
      no: right,
    };

    this.bubbles = null;
    this.nodes = null;

    // Called after every tick of the force simulation. Here we do the actual
    // repositioning of the SVG circles based on the current x and y values of
    // their bound node data.  These x and y values are modified by the force
    // simulation.
    this.ticked = () => {
      this.bubbles.attr('cx', d => d.x).attr('cy', d => d.y);
      this.handleChartSettled();
    };

    this.simulation = d3
      .forceSimulation()
      .velocityDecay(0.2)
      .force(
        'x',
        d3
          .forceX()
          .strength(forceStrength)
          .x(d => this.positions[d.option].x),
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
  }

  handleChartSettled() {
    const alphaBelowThreshold = this.simulation.alpha() < chartSettledThreshold;
    const chartJustSettled = !this.chartSettled && alphaBelowThreshold;

    if (chartJustSettled) {
      this.chartSettled = true;
      this.app.ports.chartSettled.send(null);
    }
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
        colour: d.colour,
        borderColour: d.borderColour,
        option: d.option,
        x: currentNode ? currentNode.x : Math.random() * this.width,
        y: currentNode ? currentNode.y : Math.random() * this.height,
      };
    });

    // Bind nodes data to what will become DOM elements to represent them.
    this.bubbles = this.svg
      .selectAll('.bubble')
      .data(this.nodes, d => d.personId);

    this.bubbles.exit().remove();

    // XXX Could de-duplicate these.
    const personNodeHovered = node =>
      this.app.ports.personNodeHovered.send(node.personId);

    const personNodeUnhovered = node =>
      this.app.ports.personNodeUnhovered.send(node.personId);

    const personNodeClicked = node =>
      this.app.ports.personNodeClicked.send(node.personId);

    // Create new circle elements each with class `bubble`.  There will be one
    // circle.bubble for each object in the nodes array.  Initially, their
    // radius (r attribute) will be 0.  Selections are immutable, so lets
    // capture the enter selection to apply our transition to below.
    const bubblesE = this.bubbles
      .enter()
      .append('circle')
      .classed('bubble', true)
      .classed('dim', true)
      .attr('r', 0)
      .attr('stroke-width', 2)
      .on('mouseover', personNodeHovered)
      .on('mouseout', personNodeUnhovered)
      .on('click', personNodeClicked);

    // Merge the original empty selection and the enter selection
    this.bubbles = this.bubbles.merge(bubblesE);

    // Set/update all bubble colours.
    this.bubbles
      .attr('fill', function(d) {
        return d.colour;
      })
      .attr('stroke', function(d) {
        // Use border colour if given, or generate it if not.
        return d.borderColour || d3.rgb(d.colour).darker();
      });

    // Fancy transition to make bubbles appear, ending with the correct radius
    this.bubbles
      .transition()
      .duration(2000)
      .attr('r', function(d) {
        return d.radius;
      });

    // Set the simulation's nodes to our newly created nodes array.
    this.simulation.nodes(this.nodes);

    // Reset this so we re-inform the Elm app when the chart settles again.
    this.chartSettled = false;

    // Reset the alpha value and restart the simulation
    this.simulation.alpha(1).restart();
  }
}
