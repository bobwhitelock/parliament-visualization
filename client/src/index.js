import {Main} from './Main.elm';
import BubbleChart from './BubbleChart';

const app = Main.embed(document.getElementById('root'), {
  apiUrl: process.env.ELM_APP_API_URL,
});

let chartExistenceIntervalId = null;
let chart = null;

const renderChart = data => {
  if (!chart) {
    chart = new BubbleChart(app, '#d3-simulation');
  }
  chart.setNodes(data.voteEvents, data.restartSimulation);
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
