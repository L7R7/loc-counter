import {
    Chart,
    ScatterController,
    LineElement,
    PointElement,
    LinearScale,
    TimeScale,
    Legend,
    Tooltip,
    Title,
    Filler,
    Decimation,
    Colors,
} from 'chart.js'
import 'chartjs-adapter-luxon'
import annotationPlugin from 'chartjs-plugin-annotation'

Chart.register(
    ScatterController,
    LineElement,
    PointElement,
    LinearScale,
    TimeScale,
    Legend,
    Tooltip,
    Title,
    Filler,
    Decimation,
    Colors,
    annotationPlugin
)

window.Chart = Chart
