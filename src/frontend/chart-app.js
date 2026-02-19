function initializeChart(chartData, markers) {
    if (!window.Chart) {
        console.error('Chart.js is not loaded')
        return
    }

    const data = {
        datasets: chartData,
    }

    const config = {
        type: 'scatter',
        data: data,
        options: {
            responsive: true,
            showLine: true,
            elements: {
                line: {
                    showLine: true,
                    tension: 0.2,
                },
            },
            scales: {
                x: {
                    type: 'time',
                    time: {
                        unit: 'month',
                        tooltipFormat: 'yyyy-MM-dd',
                        displayFormats: {
                            month: 'yyyy-MM',
                        },
                    },
                    title: {
                        display: true,
                        text: 'Date',
                    },
                },
                y: {
                    title: {
                        display: true,
                        text: 'Lines of Code',
                    },
                },
            },
            plugins: {
                legend: {
                    display: true,
                },
                annotation: {
                    annotations: markers,
                },
            },
        },
    }

    Object.values(config.options.plugins.annotation.annotations).forEach((annotation) => {
        annotation.enter = function ({ element }) {
            if (element && element.label && element.label.options) {
                element.label.options.display = true
                return true
            }
        }
        annotation.leave = function ({ element }) {
            if (element && element.label && element.label.options) {
                element.label.options.display = false
                return true
            }
        }
    })

    function getLanguagesFromQuery() {
        const params = new URLSearchParams(window.location.search)
        const langs = params.get('languages')
        if (!langs) return null
        return langs.split(',').map((l) => l.trim().toLowerCase())
    }

    function getFromDateFromQuery() {
        const params = new URLSearchParams(window.location.search)
        return params.get('from')
    }

    function findEarliestDate(datasets) {
        let earliest = null
        datasets.forEach((ds) => {
            if (ds.data && Array.isArray(ds.data)) {
                ds.data.forEach((point) => {
                    const pointTime = new Date(point.x).getTime()
                    if (!earliest || pointTime < earliest) {
                        earliest = pointTime
                    }
                })
            }
        })
        return earliest ? new Date(earliest) : null
    }

    function formatDateToInputValue(date) {
        const year = date.getFullYear()
        const month = String(date.getMonth() + 1).padStart(2, '0')
        const day = String(date.getDate()).padStart(2, '0')
        return `${year}-${month}-${day}`
    }

    function applyDateFilter(fromDateStr, scatterChart, originalData) {
        if (!fromDateStr) {
            // Restore all original data
            scatterChart.data.datasets.forEach((ds, index) => {
                ds.data = JSON.parse(JSON.stringify(originalData.datasets[index].data))
            })
        } else {
            const fromDateTime = new Date(fromDateStr).getTime()
            scatterChart.data.datasets.forEach((ds, index) => {
                const originalDataset = originalData.datasets[index].data
                ds.data = originalDataset.filter((point) => {
                    const pointTime = new Date(point.x).getTime()
                    return pointTime >= fromDateTime
                })
            })
        }
        scatterChart.update()
    }

    const originalData = JSON.parse(JSON.stringify(data))
    const selectedLangs = getLanguagesFromQuery()
    const fromDate = getFromDateFromQuery()
    const ctx = document.getElementById('scatterChart').getContext('2d')
    const scatterChart = new Chart(ctx, config)

    if (selectedLangs) {
        scatterChart.data.datasets.forEach((ds) => {
            ds.hidden = !selectedLangs.includes(ds.label.toLowerCase())
        })
        scatterChart.update()
    }

    const datePicker = document.getElementById('fromDatePicker')
    const earliestDate = findEarliestDate(originalData.datasets)

    if (fromDate) {
        datePicker.value = fromDate
        applyDateFilter(fromDate, scatterChart, originalData)
    } else if (earliestDate) {
        datePicker.value = formatDateToInputValue(earliestDate)
    }

    datePicker.addEventListener('change', function () {
        applyDateFilter(this.value, scatterChart, originalData)
    })

    document.getElementById('showAllBtn').onclick = function () {
        scatterChart.data.datasets.forEach((ds) => (ds.hidden = false))
        scatterChart.update()
    }

    document.getElementById('hideAllBtn').onclick = function () {
        scatterChart.data.datasets.forEach((ds) => (ds.hidden = true))
        scatterChart.update()
    }
}

window.initializeChart = initializeChart
