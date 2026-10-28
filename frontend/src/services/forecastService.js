import axios from 'axios'

// Deliberately call the Python forecasting service directly: no R/Plumber
// adapter or duplicated model registry is needed for the live forecast page.
const forecastClient = axios.create({
  baseURL: import.meta.env.VITE_TS_API_URL ?? 'http://127.0.0.1:8001',
  timeout: 600000,
  headers: { 'Content-Type': 'application/json' },
})

export async function requestLiveForecast(payload) {
  const { data } = await forecastClient.post('/live-forecast', payload)
  return data
}

export async function getForecastHistory(ticker) {
  const { data } = await forecastClient.get(`/forecast-history/${encodeURIComponent(ticker)}`)
  return data
}
