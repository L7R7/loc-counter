function initializeChart(chartData, markers) {
    if (!window.Chart) {
        console.error('Chart.js is not loaded')
        return
    }

    const fmtInt = (n) => n.toLocaleString('en-US')

    function fmtCompact(n) {
        if (n >= 1_000_000) return (n / 1_000_000).toFixed(n >= 10_000_000 ? 0 : 2) + 'M'
        if (n >= 1_000) return (n / 1_000).toFixed(n >= 10_000 ? 0 : 1) + 'k'
        return String(n)
    }

    const parseDate = (s) => new Date(s + 'T00:00:00')

    function getLanguagesFromQuery() {
        const langs = new URLSearchParams(window.location.search).get('languages')
        if (!langs) return null
        return langs.split(',').map((l) => l.trim().toLowerCase())
    }

    const accent = window.getComputedStyle(document.documentElement).getPropertyValue('--accent').trim()
    const accentText =
        window.getComputedStyle(document.documentElement).getPropertyValue('--accent-text').trim() || accent

    chartData.forEach((ds) => {
        ds.pointRadius = 0
        ds.pointHoverRadius = 4
        ds.borderWidth = 2
        ds.tension = 0.25
    })

    let earliest = null
    chartData.forEach((ds) =>
        ds.data.forEach((p) => {
            const t = parseDate(p.x).getTime()
            if (earliest === null || t < earliest) earliest = t
        })
    )

    let chart

    function initChart() {
        const ctx = document.getElementById('scatterChart').getContext('2d')

        Object.values(markers).forEach((a) => {
            a.borderColor = accent
            a.borderWidth = 2
            a.borderDash = [5, 4]
            a.label = a.label || {}
            a.label.display = false
            a.label.position = 'start'
            a.label.backgroundColor = accent
            a.label.color = '#fff'
            a.label.font = { size: 11, weight: '600' }
            a.label.padding = 6
            a.label.borderRadius = 6
            a.enter = ({ element }) => {
                if (element?.label?.options) {
                    element.label.options.display = true
                    return true
                }
            }
            a.leave = ({ element }) => {
                if (element?.label?.options) {
                    element.label.options.display = false
                    return true
                }
            }
        })

        chart = new window.Chart(ctx, {
            type: 'scatter',
            data: { datasets: chartData },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                showLine: true,
                interaction: { mode: 'nearest', intersect: false },
                scales: {
                    x: {
                        type: 'time',
                        time: { unit: 'month', tooltipFormat: 'yyyy-MM-dd', displayFormats: { month: 'yyyy-MM' } },
                        grid: { color: 'rgba(15,23,42,0.05)' },
                        border: { color: 'rgba(15,23,42,0.12)' },
                        ticks: { color: '#94a3b8', font: { size: 11 } },
                    },
                    y: {
                        grid: { color: 'rgba(15,23,42,0.05)' },
                        border: { display: false },
                        ticks: {
                            color: '#94a3b8',
                            font: { size: 11 },
                            callback: (v) => fmtCompact(v),
                        },
                    },
                },
                plugins: {
                    legend: { display: false },
                    tooltip: {
                        backgroundColor: '#0f172a',
                        borderColor: 'rgba(255,255,255,0.08)',
                        borderWidth: 1,
                        padding: 10,
                        cornerRadius: 8,
                        titleFont: { size: 12 },
                        bodyFont: { size: 12 },
                        callbacks: {
                            label: (c) => ` ${c.dataset.label}: ${fmtInt(c.parsed.y)}`,
                        },
                    },
                    annotation: { annotations: markers },
                },
            },
        })
    }

    function syncLangRows() {
        document.querySelectorAll('.lang-row').forEach((row) => {
            row.classList.toggle('off', !!chartData[Number(row.dataset.index)].hidden)
        })
    }

    function toggleLang(index) {
        chartData[index].hidden = !chartData[index].hidden
        chart.update()
        syncLangRows()
    }

    function wireLangRows() {
        document.querySelectorAll('.lang-row').forEach((row) => {
            row.addEventListener('click', () => toggleLang(Number(row.dataset.index)))
        })
    }

    function highlightMarker(key, on) {
        const ann = chart.options.plugins.annotation.annotations[key]
        if (!ann) return
        ann.label.display = on
        ann.borderWidth = on ? 3 : 2
        ann.borderColor = on ? accentText : accent
        chart.update('none')
    }

    function wireMilestones() {
        document.querySelectorAll('.ms-row').forEach((row) => {
            const key = row.dataset.key
            row.addEventListener('mouseenter', () => highlightMarker(key, true))
            row.addEventListener('mouseleave', () => highlightMarker(key, false))
        })
    }

    const originalData = chartData.map((ds) => ds.data.map((p) => ({ ...p })))

    function applyDateFilter(fromStr) {
        if (!fromStr) {
            chartData.forEach((ds, i) => (ds.data = originalData[i].map((p) => ({ ...p }))))
            chart.options.scales.x.min = undefined
        } else {
            const from = parseDate(fromStr).getTime()
            chartData.forEach((ds, i) => {
                ds.data = originalData[i].filter((p) => parseDate(p.x).getTime() >= from)
            })
            chart.options.scales.x.min = from
        }
        chart.update()
    }

    function wireControls() {
        const picker = document.getElementById('fromDatePicker')
        const fromDate = new URLSearchParams(window.location.search).get('from')

        if (picker) {
            if (fromDate) {
                picker.value = fromDate
                applyDateFilter(fromDate)
            } else if (earliest) {
                picker.value = new Date(earliest).toLocaleDateString('en-CA')
            }
            picker.addEventListener('change', function () {
                applyDateFilter(this.value)
            })
        }

        document.getElementById('showAllBtn').addEventListener('click', () => {
            chartData.forEach((ds) => (ds.hidden = false))
            chart.update()
            syncLangRows()
        })
        document.getElementById('hideAllBtn').addEventListener('click', () => {
            chartData.forEach((ds) => (ds.hidden = true))
            chart.update()
            syncLangRows()
        })
    }

    initChart()

    const selectedLangs = getLanguagesFromQuery()

    if (selectedLangs) {
        chartData.forEach((ds) => (ds.hidden = !selectedLangs.includes(ds.label.toLowerCase())))
        chart.update()
    }

    wireLangRows()
    wireMilestones()
    wireControls()
    syncLangRows()
}

window.initializeChart = initializeChart
