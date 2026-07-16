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

    const css = (name) => window.getComputedStyle(document.documentElement).getPropertyValue(name).trim()
    const palette = () => ({
        accent: css('--accent'),
        accentText: css('--accent-text') || css('--accent'),
        grid: css('--chart-grid'),
        axis: css('--chart-axis'),
        tick: css('--chart-tick'),
        tooltipBg: css('--chart-tooltip-bg'),
        tooltipText: css('--chart-tooltip-text'),
    })
    let P = palette()

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
            a.borderColor = P.accent
            a.borderWidth = 2
            a.borderDash = [5, 4]
            a.label = a.label || {}
            a.label.display = false
            a.label.position = 'start'
            a.label.backgroundColor = P.accent
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
                        grid: { color: P.grid },
                        border: { color: P.axis },
                        ticks: { color: P.tick, font: { size: 11 } },
                    },
                    y: {
                        grid: { color: P.grid },
                        border: { display: false },
                        ticks: {
                            color: P.tick,
                            font: { size: 11 },
                            callback: (v) => fmtCompact(v),
                        },
                    },
                },
                plugins: {
                    legend: { display: false },
                    tooltip: {
                        backgroundColor: P.tooltipBg,
                        titleColor: P.tooltipText,
                        bodyColor: P.tooltipText,
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
        ann.borderColor = on ? P.accentText : P.accent
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

    // re-read CSS colours and repaint the chart when the theme flips
    function applyChartTheme() {
        P = palette()
        const o = chart.options
        o.scales.x.grid.color = o.scales.y.grid.color = P.grid
        o.scales.x.border.color = P.axis
        o.scales.x.ticks.color = o.scales.y.ticks.color = P.tick
        o.plugins.tooltip.backgroundColor = P.tooltipBg
        o.plugins.tooltip.titleColor = o.plugins.tooltip.bodyColor = P.tooltipText
        Object.values(markers).forEach((a) => {
            a.borderColor = P.accent
            a.label.backgroundColor = P.accent
        })
        chart.update('none')
    }
    document.addEventListener('themechange', applyChartTheme)

    // theme toggle: Auto → Light → Dark → …
    function wireThemeToggle() {
        const btn = document.getElementById('themeToggle')
        if (!btn || !window.getThemePref) return
        const icons = {
            auto: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="2" y="3" width="20" height="14" rx="2"/><path d="M8 21h8M12 17v4"/></svg>',
            light: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="4"/><path d="M12 2v2M12 20v2M2 12h2M20 12h2M5 5l1.5 1.5M17.5 17.5 19 19M19 5l-1.5 1.5M6.5 17.5 5 19"/></svg>',
            dark: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M21 12.8A9 9 0 1 1 11.2 3 7 7 0 0 0 21 12.8Z"/></svg>',
        }
        const order = ['auto', 'light', 'dark']
        const render = () => {
            const cur = window.getThemePref()
            btn.innerHTML = icons[cur] + cur.charAt(0).toUpperCase() + cur.slice(1)
        }
        btn.addEventListener('click', () => {
            window.setThemePref(order[(order.indexOf(window.getThemePref()) + 1) % order.length])
            render()
        })
        render()
    }

    initChart()
    wireThemeToggle()

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
